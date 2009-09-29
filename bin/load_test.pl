#!perl -w

use strict;
use warnings;

use Template;

use Chart::Clicker;
use Chart::Clicker::Data::Series;
use Chart::Clicker::Data::DataSet;

use Getopt::Long;
use DateTime;
use File::Path;

use Data::Dumper;

use FindBin qw($Bin);

my @tests = (
    {
        name => "objects",
        description => "Fetch all objects",
        url => "/objects/",
        method => "GET",
        data => "",
        rate => "200",
    },
    {
        name => "object_probes",
        description => "Fetch all object probes",
        url => "/object/1/probes",
        method => "GET",
        data => "",
        rate => "2000",
    }
);

my $testname;
my $probix_host = "localhost";
my $probix_port = "8000";
my $reports_dir = $Bin."/../load_test/reports";
my $help = 0;
my $list = 0;

my $debug = 0;
    
GetOptions("run=s" => \$testname,
           "server=s" => \$probix_host,
           "port=i" => \$probix_port,
           "d|reports_dir=s" => \$reports_dir,
           "help" => \$help,
           "list" => \$list,
           "debug" => \$debug,
           ) or usage();
my $output_path = join "/", ($reports_dir, DateTime->now());

usage() if ($help);

list_tests() if ($list);

unless ($testname) {
    print "\n  No test specified. Use --list to see available tests\n\n";

    usage();
}

run_test($testname);

sub list_tests {
    print "\nList of all available load tests. Use test name in --run argument\n";

    for my $test ( @tests ) {
        print <<EOF;
        
$test->{name}     $test->{description}
                  URL:    $test->{url}
                  method: $test->{method}
                  data:   $test->{data}
        
EOF
    }
    exit;
}

sub run_test {
    my ($testname) = @_;
    
    my ($test) = grep { $_->{name} eq $testname} @tests;

    unless ($test) {
        print "No test with name $testname found. Use --list to see complete list\n";
        exit;
    }
  
    my $max_rate = $test->{rate};
    my $min_rate = $test->{rate} / 20;
    my $rate_step = $test->{rate} / 20;
    
    my @results;
    for ( my $rate = $min_rate; $rate <= $max_rate; $rate += $rate_step ) {
        $test->{rate} = $rate;
        print "Running test with rate: $rate\n";
        my $result = run_httperf($test);
        $result->{rate} = $rate;
        push @results, $result;
    }

    mkpath($output_path);
    output_charts($test->{name},\@results);
    output_report($test,\@results);
}

sub output_charts {
    my ($name, $results) = @_;

    my @keys = map { $_->{rate} } @$results;
    ## reply time chart
    my @reply_time_values = map { $_->{reply_time} } @$results;
    my $reply_time_series = Chart::Clicker::Data::Series->new(
        name    => "Reply time, ms",
        keys    => [ @keys ],
        values  => [ @reply_time_values ],
    );
    my $reply_time_dataset = Chart::Clicker::Data::DataSet->new(
        series  => [ $reply_time_series ],
    );

    my $reply_time_chart = Chart::Clicker->new;
    $reply_time_chart->add_to_datasets($reply_time_dataset);
   
    $reply_time_chart->write_output("$output_path/${name}_reply_time.png");

    ## building reply rate chart
    ##
    my @reply_rate_values = map { $_->{reply_rate}->{avg} } @$results;
    my $reply_rate_series = Chart::Clicker::Data::Series->new(
        name    => "Reply rate, req/s",
        keys    => [ @keys ],
        values  => [ @reply_rate_values ],
        );

    my $reply_rate_dataset = Chart::Clicker::Data::DataSet->new(
        series  => [ $reply_rate_series ],
    );

    my $reply_rate_chart = Chart::Clicker->new;
    $reply_rate_chart->add_to_datasets($reply_rate_dataset);
    $reply_rate_chart->write_output("$output_path/${name}_reply_rate.png");

    my @error_timeout_values = map { $_->{error_timeout} } @$results;
    my $error_timeout_series = Chart::Clicker::Data::Series->new(
        name    => "Timeout errors", 
        keys    => [ @keys ],
        values  => [ @error_timeout_values ],
        );

    my @error_total_values = map { $_->{error_total} } @$results;
    my $error_total_series = Chart::Clicker::Data::Series->new(
        name    => "Total errors", 
        keys    => [ @keys ],
        values  => [ @error_total_values ],
        );


    my $error_dataset = Chart::Clicker::Data::DataSet->new(
        series  => [ $error_timeout_series, $error_total_series ],
    );

    my $error_chart = Chart::Clicker->new;
    $error_chart->add_to_datasets($error_dataset);
    $error_chart->write_output("$output_path/${name}_error.png");

}

sub output_report {
    my ( $test, $results ) = @_;
    
    my $vars = {
        host         => "$probix_host:$probix_port",
        url          => $test->{url},
        name         => $test->{name},
        rate         => $test->{rate},
        description  => $test->{description},
        cmd          => $results->[$#{$results}]->{cmd},
        results      => $results
            
    };

    my $tt = Template->new({RELATIVE => 1, ABSOLUTE => 1}) || die $Template::ERROR, "\n";
    $tt->process("$Bin/../load_test/report.tmpl", $vars, "$output_path/report.html")
        || die $tt->error(), "\n";

    print "Saved report to $output_path\n";
}

sub run_httperf {
    my ($params) = @_;

    my $conns = $params->{rate}*10;
    my $cmd = "httperf --hog --server=$probix_host --port=$probix_port --uri=$params->{url} --rate=$params->{rate} --method=$params->{method} --num-conns=$conns  --num-calls=1 --timeout 5";

    my $output = `$cmd`;
    
    my $result = {
        cmd => $cmd,
        rate => $params->{rate}
    };

    print $output if $debug;

    ($result->{conns},
     $result->{reqs},
     $result->{replies}) =
         $output =~ /Total: connections (\d+) requests (\d+) replies (\d+)/;

    ($result->{req_rate}) =
        $output =~ /Request rate: ([\d.]+)/;

    ($result->{reply_rate}->{min},
     $result->{reply_rate}->{avg},
     $result->{reply_rate}->{max}) =
         $output =~ /Reply rate.+?min ([\d.]+) avg ([\d.]+) max ([\d.]+)/;

    ($result->{reply_time}) =
        $output =~ /Reply time.+response ([\d.]+)/;

    ($result->{error_timeout}) =
        $output =~ /client-timo ([\d.]+)/;
    
    ($result->{error_total}) =
        $output =~ /Errors: total ([\d.]+)/;

    print Dumper($result) if $debug;
    return $result;
}

sub usage {
	print <<EOF;
  Usage: $FindBin::Script <options>

  This program creates load on server and creates report.
  --help           print this help
  --list           lists load tests and exist
  --run testname   runs test with name 'testname'
  --server         host to run test against. Default: localhost
  --port           port of server. Defalut: 8000
  -d|--reports_dir    specifies which directory should be used for reports. Default: ../load_test/reports/
  --debug          show debug output
EOF
exit;
}
