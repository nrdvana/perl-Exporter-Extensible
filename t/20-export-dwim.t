#! /usr/bin/env perl
use strict;
use warnings;
no warnings 'once', 'redefine';
use Test::More;
use Scalar::Util 'weaken';

use_ok( 'Exporter::Extensible' ) or BAIL_OUT;
ok( eval q{
	package Example;
	use Exporter::Extensible -exporter_setup => 1;
	
	sub alpha { 'a' }
	sub beta  { 'b' }
	sub gamma { 'g' }
	sub delta { 'd' }
	sub _generate_delta { sub { 'd' } }
	sub _generateArray_epsillon { [ 'e' ] }
	sub _generateGlob_zeta { open my $fh, '>+', \""; $fh; }
	our $scalar0;
	our $scalar1= \"test";
	our $scalar2= [ 'test' ];
	our $scalar3= bless {}, "Foo";
	our $scalar4= sub { 1 };
	our $scalar5= \sub { 1 };
	our @array;
	our %hash;
	our ($glob, @glob, %glob); sub glob { "glob" };
	
	1;
}, 'declare Example' ) or diag $@;

my @tests= (
	[ 'export a sub',
		['alpha'],
		{ alpha => Example->can('alpha') }
	],
	[ 'export a scalar',
		['$scalar0'],
		{ '$scalar0' => eval '*Example::scalar0{SCALAR}' },
	],
	[ 'export a scalar holding a scalar ref',
		['$scalar1'],
		{ '$scalar1' => eval '*Example::scalar1{SCALAR}' },
	],
	[ 'export a scalar holding an array ref',
		['$scalar2'],
		{ '$scalar2' => eval '*Example::scalar2{SCALAR}' },
	],
	[ 'export a scalar holding a blesed object',
		['$scalar3'],
		{ '$scalar3' => eval '*Example::scalar3{SCALAR}' },
	],
	[ 'export a scalar holding a coderef',
		['$scalar4'],
		{ '$scalar4' => eval '*Example::scalar4{SCALAR}' },
	],
	[ 'export a scalar holding a coderef-ref',
		['$scalar5'],
		{ '$scalar5' => eval '*Example::scalar5{SCALAR}' },
	],
	[ 'export a sub',
		['alpha'],
		{ alpha => Example->can('alpha') },
	],
	[ 'export an array',
		['@array'],
		{ '@array' => eval '*Example::array{ARRAY}' },
	],
	[ 'export a hash',
		['%hash'],
		{ '%hash' => eval '*Example::hash{HASH}' },
	],
	[ 'export a typeglob',
		['*glob'],
		{ '*glob' => eval '\*Example::glob' },
	],
	[ 'export a group',
		['alpha','beta','gamma',':group1' => ['alpha','beta','gamma']],
		{ map { $_ => Example->can($_) } qw( alpha beta gamma ) },
		{ 'group1' => [qw( alpha beta gamma )] },
	],
	[ 'export a generator',
		['=delta'],
		{ delta => '_generate_delta' },
	],
	[ 'export a generator - 2',
		['=delta' => \'_generate_delta' ],
		{ delta => '_generate_delta' },
	],
	[ 'export an array generator',
		['=@epsillon'],
		{ '@epsillon' => '_generateArray_epsillon' },
	],
	[ 'export a typeglob generator',
		['=*zeta'],
		{ '*zeta' => '_generateGlob_zeta' },
	],
	[ 'export an option',
		['-gamma'],
		{ -gamma => 'gamma' },
	],
	#[ 'export an option with args',
	#	['-alpha(2)','-beta(*)','-delta(?)'],
	#	{ -alpha => [ alpha => 2 ], -beta => [ beta => '*' ], -delta => [ delta => '?' ] },
	#],
);
for (@tests) {
	my ($name, $args, $expected, $expected_tags)= @$_;
	%Example::EXPORT= %Example::EXPORT_TAGS= ();
	eval 'package Example; export(@$args); 1' or die $@;
	is_deeply( \%Example::EXPORT, $expected || {}, "$name - EXPORT" );
	is_deeply( \%Example::EXPORT_TAGS, $expected_tags || {}, "$name - EXPORT_TAGS" );
}

done_testing;
