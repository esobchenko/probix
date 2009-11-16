#!perl

use strict;
use warnings;

use LWP::UserAgent;
use Data::Dumper;
use JSON::Any;

my $probix_host = "http://127.0.0.1:8000";

my $ua = LWP::UserAgent->new();

my $probix_url = URI->new($probix_host."/series");
$probix_url->query_form({label =>"CPU load average"});

my $result = $ua->post($probix_url);
my $object_url = $result->header("Location");
my $json_encoder = JSON::Any->new();

while (1) {
	my @values = split m{, }, (`w` =~ /load averages?: (.+)/)[0];
	my $json = $json_encoder->encode({ timestamp => time(), value => $values[0]});
	$ua->post($probix_host.$object_url, Content => $json );
	print "url: ", Dumper($probix_host.$object_url);
	print "posting value: ", Dumper($json);
	sleep(60);
}
