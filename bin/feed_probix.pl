#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use Readonly;

use LWP::UserAgent;
use LWP::ConnCache;
use JSON::Any;

Readonly my $probix_uri => qq'http://127.0.0.1:8000/';
Readonly my $objects_to_create => 1000;
Readonly my $probes_to_create => 10000;

sub mk_create_object_req {
	my $json = shift;
	HTTP::Request->new(
		'POST',
		$probix_uri . "/object",
		[],
		$json
	);
}

sub mk_create_probe_req {
	my ($id_object, $json) = @_;
	HTTP::Request->new(
		'POST',
		$probix_uri . "/object/$id_object/probes",
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

# object template
Readonly my $object_t => qq/
	{
		"name": "%s",
		"info": "%s"
	}
/;

# probe template
Readonly my $probe_t => qq/
	{
		"timestamp": %s,
		"value": %s
	}
/;

# for measuring average feeding speed
my ($start_time, $end_time, $avg);

# flush stdout
$|++;

#
# feeding objects
#
my @objects = (); # newly created objects
print "feeding objects... ";
$start_time = time();
for (1..$objects_to_create) {
	my $object = sprintf $object_t, "foo", "bar";
	my $create_object_req = mk_create_object_req($object);
	push @objects, perform_req($create_object_req);
}
$end_time = time();
$avg = int ( $objects_to_create / ($end_time - $start_time) );
print "done ($avg objects/sec);\n";

#
# feeding probes
#
print "feeding probes... ";
$start_time = time();
for (1..$probes_to_create) {
	my $object = $objects[rand @objects];
	my $probe = sprintf $probe_t, time(), $_;
	my $request = mk_create_probe_req($object->{id}, $probe);
	perform_req($request);
}
$end_time = time();
$avg = int ( $probes_to_create / ($end_time - $start_time) );
print "done ($avg probes/sec);\n";

