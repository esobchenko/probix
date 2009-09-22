#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;

use LWP::UserAgent;
use LWP::ConnCache;
use JSON::Any;

# program defaults
my $objects = 1000;
my $probes = 10000;
my $help = 0;

my $prog = $0;
$prog =~ s@.*/@@;

GetOptions("objects=i" => \$objects, "probes=i" => \$probes, "help" => \$help) or usage(1);
usage(0) if ($help || !@ARGV);

my $server_uri = shift @ARGV;

# object template
my $object_t = qq/
	{
		"name": "%s",
		"info": "%s"
	}
/;

# probe template
my $probe_t = qq/
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
my @objects = (); # for newly created objects
print "feeding $objects objects... ";
$start_time = time();
for (1..$objects) {
	my $object = sprintf $object_t, "foo", "bar";
	my $req = mk_create_object_req($object);
	push @objects, perform_req($req);
}
$end_time = time();
$avg = int ( $objects /
	($end_time == $start_time ? 1 : $end_time - $start_time) # to avoid division by zero
);
print "done ($avg objects/sec);\n";

#
# feeding probes
#
print "feeding $probes probes... ";
$start_time = time();
for (1..$probes) {
	my $object = $objects[rand @objects];
	my $probe = sprintf $probe_t, time(), $_;
	my $req = mk_create_probe_req($object->{id}, $probe);
	perform_req($req);
}
$end_time = time();
$avg = int ( $probes /
	($end_time == $start_time ? 1 : $end_time - $start_time) # to avoid division by zero
);
print "done ($avg probes/sec);\n";

sub usage {
	my $status = shift;
	print <<EOF;
usage: $prog [options] server_uri

  This program feeds probix server with test data.

  --objects    number of objects to feed (default: $objects)
  --probes     number of probes to feed (default: $probes)
  --help       print this help

EOF

	exit($status);
}

sub mk_create_object_req {
	my $json = shift;
	HTTP::Request->new(
		'POST',
		$server_uri . "/object",
		[],
		$json
	);
}

sub mk_create_probe_req {
	my ($id_object, $json) = @_;
	HTTP::Request->new(
		'POST',
		$server_uri . "/object/$id_object/probes",
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
	die( sprintf qq/Request failed: %s\n/, $res->status_line ) unless $res->is_success;
	JSON::Any->jsonToObj( $res->content ) if $res->content;
}

