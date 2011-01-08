#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;

use LWP::UserAgent;
use LWP::ConnCache;
use JSON::Any;

# program defaults
my $series = 1000;
my $ticks = 10000;
my $help = 0;

my $prog = $0;
$prog =~ s@.*/@@;

GetOptions("series=i" => \$series, "ticks=i" => \$ticks, "help" => \$help) or usage(1);
usage(0) if ($help || !@ARGV);

my $server_uri = shift @ARGV;

# probe template
my $tick_t = qq/
	{
		"timestamp": %s,
		"value": %s
	}
/;

# for stdout messages to appear immediately
$|++;

# for measuring average feeding speed
my ($start_time, $end_time, $avg);

#
# feeding objects
#
my @series = (); # for newly created objects
print "feeding $series objects... ";
$start_time = time();
for (1..$series) {
	my $req = mk_create_series_req("foo");
	push @series, perform_req($req);
}
$end_time = time();
$avg = int ( $series /
	($end_time == $start_time ? 1 : $end_time - $start_time) # to avoid division by zero
);
print "done ($avg series/sec);\n";

#
# feeding probes
#
print "feeding $ticks ticks... ";
$start_time = time();
for (1..$ticks) {
	my $s = $series[rand @series];
	my $t = sprintf $tick_t, time(), $_;
	my $req = mk_create_tick_req($s->{id}, $t);
	perform_req($req);
}
$end_time = time();
$avg = int ( $ticks /
	($end_time == $start_time ? 1 : $end_time - $start_time) # to avoid division by zero
);
print "done ($avg ticks/sec);\n";

sub usage {
	my $status = shift;
	print <<EOF;
usage: $prog [options] server_uri

  This program feeds probix server with test data.

  --series    number of objects to feed (default: $series)
  --ticks     number of probes to feed (default: $ticks)
  --help       print this help

EOF

	exit($status);
}

sub mk_create_series_req {
	my $label = shift;
	return HTTP::Request->new(
		'POST',
		$server_uri . "/series",
		[ label => $label],
		undef
	);
}

sub mk_create_tick_req {
	my ($id_object, $json) = @_;
	return HTTP::Request->new(
		'POST',
		$server_uri . "/series/$id_object",
		[],
		$json
	);
}

sub perform_req {
	my $r = shift;
	my $ua = LWP::UserAgent->new(
		conn_cache => LWP::ConnCache->new(total_capacity => undef)
	);
	$ua->agent("megaclient/0.1");
	$ua->env_proxy;

	my $res = $ua->request( $r );
	die( sprintf qq/Request failed: %s\n/, $res->status_line ) unless $res->is_success || $res->is_redirect;
	return JSON::Any->jsonToObj( $res->content ) if $res->content;
	return;
}

