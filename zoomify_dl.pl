#!/usr/bin/perl

my $Version = '0.1';
my $Copyright = 'zoomify_dl.pl Copyright 2010 by Jim Leonard (jim@xuth.net)';

# zoomify_dl.pl
# 
# A utility to download images that have saved for viewing with the "Zoomify" flash 
# based web viewer.  Amongst other options you can specify which zoom level to 
# download (so you can download intermediate sizes) or just get information on 
# the zoomified image without downloading it.

# bugs / missing features: 
    # BUG_TILEGROUP_SIZE
    # some sources say there are tilesize images per tilegroup, others just say 256... 
    # I don't know which is right.  This should be relatively simple to investigate though.

    # BUG_MORE_THAN_ONE_IMAGE
    # This doesn't handle more than one image per zoominfo directory.

    # BUG_DISABLE_HTML_CHECK_OPTION
    # should create an option to disable the content type check

    # BUG_ADD_RETRY_LOGIC
    # should add retry logic (and associated options) when we
    # can't get a tile for some reason

    # BUG_ADD_JPEG_QUALITY
    # add an option to save at a different jpeg quality/compression level.

    # BUG_ALLOW_OTHER_FILE_TYPES
    # allow saving as something other than a jpeg.

    # BUG_HUGE_IMAGES
    # come up with some manner of handling really _huge_ images that most 
    # image formats don't support.

    # BUG_CLEAN_TMP_FILES_ON_SIGINT
    # if imagemagick is told to restrict its ram usage it will create temp files
    # in /tmp or elsewhere.  These are not cleaned up on SIGINT (<ctrl> c) or
    # on some errors.

    # BUG_NO_ALPHA_CHANNEL
    # We don't need the alpha channel.  Does imagemagick always allocate one?
    # I think there's a setting to get rid of it and realize a 25% memory savings.
    # Similarly it would be nice to only use only 8 bits of color depth rather than
    # 16 bits that IM currently uses.

    # BUG_ENABLE_HTTP_COOKIES
    # BUG_ENABLE_HTTP_OPTIONS
    # enable more http options like cookies, username/password, alternate headers.

    # BUG_WONT_DOWNLOAD_TWICE
    # this won't download the same image twice.  Normally this is a feature, but
    # someone might want to download different sections of the same image.

use strict;
use warnings;

use LWP 5.64;
use LWP::ConnCache;

use HTML::LinkExtor;
use URI::URL;

use POSIX qw(ceil floor);

use Image::Magick;

my $browser = LWP::UserAgent->new;
$browser->conn_cache(LWP::ConnCache->new());
# BUG_ENABLE_HTTP_COOKIES
# BUG_ENABLE_HTTP_OPTIONS
# enable more http options like cookies, username/password, alternate headers.

my %images_dled;   #images downloaded.  Easy to find the same images multiple times on page scans.
my %html_dled;     #html pages downloaded.  Reset for each recursive search.

my %optargs;
$optargs{zoomify_level} = 'max';
$optargs{use_alt_name} = 0;
$optargs{alt_name} = "alt_name";
$optargs{display_properties} = 0;
$optargs{no_download} = 0; 
$optargs{quiet} = 0;
$optargs{savetiles} = 0;
$optargs{noimage} = 0;
$optargs{find_in_html} = 0;
$optargs{debugging} = 0;
$optargs{positive_filters} = [];
$optargs{negative_filters} = [];
$optargs{use_alt_geometry} = 0;
$optargs{alt_geometry} = "";


while (my $url = shift(@ARGV)) {
    if ($url =~ /^([\-\+])(\w)(.*)$/) {
	# handle options
	my $optval = $1;
	my $opt = $2;
	my $arg = $3;
	my $optnum = ($optval eq "-") ? 1 : 0;

	if ($opt eq "h" || $opt eq "?") {
	    print_help();
	} elsif ($opt eq "v") {
	    print "$0 version $Version\n";
	    print "$Copyright\n";
	} elsif ($opt eq "f") {
	    $optargs{use_alt_name} = $optnum;
	    $optargs{alt_name} = $arg;
	} elsif ($opt eq "c") {
	    $optargs{use_alt_geometry} = $optnum;
	    $optargs{alt_geometry} = $arg if $optnum;
	} elsif ($opt eq "d") {
	    $optargs{display_properties} = $optnum;
	} elsif ($opt eq "g") {
	    $optargs{debugging} = $optnum;
	} elsif ($opt eq "m") {
	    if ($optnum) {
		die "invalid or unspecified memory limit." unless $arg > 0;
		$optargs{memory_limit} = $arg;
	    } else {
		$optargs{memory_limit} = 0;
	    }
	} elsif ($opt eq "n") {
	    $optargs{no_download} = $optnum;
	} elsif ($opt eq "q") {
	    $optargs{quiet} = $optnum;
	} elsif ($opt eq "r") {
	    if ($optnum && $arg) {
		$optargs{find_in_html} = $arg;
	    } else {
		$optargs{find_in_html} = $optnum;
	    }
	} elsif ($opt eq "t") {
	    $optargs{savetiles} = $optnum;
	} elsif ($opt eq "x") {
	    $optargs{noimage} = $optnum;
	} elsif ($opt eq "z") {
	    if ($optval eq '-') {
		$optargs{zoomify_level} = $arg;
	    } else {
		$optargs{zoomify_level} = 'max';
	    }
	} elsif ($opt eq "F") {
	    if ($optnum) {
		push @{$optargs{positive_filters}}, $arg;
	    } else {
		$optargs{positive_filters} = [];
	    }
	} elsif ($opt eq "N") {
	    if ($optnum) {
		push @{$optargs{negative_filters}}, $arg;
	    } else {
		$optargs{negative_filters} = [];
	    }
	} else {
	    die "unknown argument \"$opt\"\n";
	}
	next;
    }

    %html_dled = ();
    download($url, $optargs{find_in_html}, 1);
#    zoomify_dl($url);
    $optargs{use_alt_name} = 0;
}

sub print_help {
    ###   "         1111111111222222222233333333334444444444555555555566666666667777777777
    ###   "1234567890123456789012345678901234567890123456789012345678901234567890123456789
    print "usage: $0 [options] [url] [more options] [more urls...]\n";
    print " Download a full zoomified image.\n";
    print "\n";
    print " Options:\n";
    print "   Options modify the behavior of $0 for any urls listed after the\n";
    print "   option and stay in effect until negated.  Options can be negated\n";
    print "   using a '+' instead of '-' when specifing the option.\n";
    print "\n";
    print "   -h, -?         This useful help screen.\n";
    print "   -v             Print version and copyright information.\n";
    print "\n";
    print "   -d             Display image and zoomify properties.\n";
    print "   -q             Quiet mode.  Suppresses normal outputs but doesn't\n";
    print "                  suppress requested outputs.\n";
    print "   -g             Print debugging information (good for debugging filters).\n";
    print "   -m<MB>         Restrict memory usage by ImageMagick to <MB> megabytes.\n";
    print "\n";
    print "   -z<level>      Download a specific zoom level.  Defaults to max.\n";
    print "   -f<filename>   Base filename to use for tiles and final filename.\n";
    print "                  Defaults to the final directory in the url.  This\n";
    print "                  option only affects the first url following this option.\n";
    print "   -n             Don't actually download the image tiles.  Gets the\n";
    print "                  image properties.  Useful with -d option.\n";
    print "   -t             Save the image tiles to individual files.\n";
    print "   -x             Don't save a merged image file.\n";
    print "   -c<geometry>   \"Crop\" the image to a specified <geometry>.  The format\n";
    print "                  for <geometry> is <width>[%]x<height>[%]+<x>[%]+<y>[%].\n";
    print "                  Each element defaults to pixels but can be specified as\n";
    print "                  a percentage if '%' is appended to it.  Thus the\n";
    print "                  following are all valid:\n";
    print "                      -c1000x700+100+0\n";
    print "                      -c50%x50%+25%+25%\n";
    print "                      -c800x41.2%+30%+100\n";
    print "\n";
    print "   -r[<level>]    Search html pages for zoomifyImagePath's.  If level is\n";
    print "                  specified allows recursively searching html pages for\n";
    print "                  based on <level>.  If level is 0, the url is assumed to\n";
    print "                  be a zoomified image path url (default behavior without\n";
    print "                  -r).  If level is 1 then get an html page and search it\n";
    print "                  for image paths (behavior of -r without further args.  At\n";
    print "                  level 2 search a page and all pages that it links to for\n";
    print "                  image paths.  This is good for index or thumbnail pages.\n";
    print "                  Level 3, search a page, its links and the links to\n";
    print "                  those pages.  Etc.  THIS IS POTENTIALLY VERY EVIL.  Please\n";
    print "                  use this option with care and discretion and possibly\n";
    print "                  use a low zoom level like -z2 or -z3.\n";
    print "    -F<regex>     Recursively found urls must match the perl regex for\n";
    print "                  the link to be followed.  Multiple regexes can be\n";
    print "                  specified (must match all) with multiple -F instances.\n";
    print "    -N<regex>     Recursively found urls must NOT match the perl regex for\n";
    print "                  the link to be followed.  Multiple regexes can be\n";
    print "                  specified (must not match any) with multiple -N instances.\n";
    print "\n";
}


sub download {
    my ($url, $r_level, $ignore_filters) = @_;
    
    unless ($r_level) {
	zoomify_dl($url);
	return;
    }
    
    if (exists($html_dled{$url})) {
	print "already looked at $url\n" if $optargs{debugging};
	return;
    }
    $html_dled{$url} = 1;


    # we ignore any url filters on any url specifically handed to us.
    unless ($ignore_filters) {
	foreach my $filter (@{$optargs{positive_filters}}) {
	    unless ($url =~ /$filter/) {
		print "$url fails filter $filter\n" if $optargs{debugging};
		return;
	    }
	}
	
	foreach my $filter (@{$optargs{negative_filters}}) {
	    if ($url =~ /$filter/) {
		print "$url fails negative filter $filter\n" if $optargs{debugging};
		return;
	    }
	}
    }
	    

    # BUG_DISABLE_HTML_CHECK_OPTION
    # should create an option to disable the content type check

    print "examining $url\n" if $optargs{debugging};
    # make sure we're getting an html page
    my $response = $browser->head($url);
    unless ($response->is_success) {
	warn "Failed to get headers for $url.";
	return;
    }

    unless ($response->content_type eq 'text/html') {
	print "$url is not of type 'text/html'\n" if $optargs{debugging};
	return;
    }

    print "getting $url\n" if $optargs{debugging};
    # now download the url
    $response = $browser->get($url);
    unless ($response->is_success) {
	warn "Failed to get $url.";
	return;
    }
    
    my $page = $response->content;
    # first let's search for zoomified urls
    while($page =~ /zoomifyImagePath=([^"'&]*)/g) {
	my $zurl = URI->new_abs($1, $url);
	zoomify_dl($zurl);
    }

    # check our recursion level
    # don't get any more urls if we're at level 1 since level 0 must be a zoomified path
    return unless $r_level > 1;

    my $parser = HTML::LinkExtor->new(undef, $url);
    $parser->parse($page);
    my @links = $parser->links;
    foreach my $linkelement (@links) {
	for (my $i = 2; $i <= $#{$linkelement}; $i += 2) {
	    print "following link to $linkelement->[$i] found in $url\n" if $optargs{debugging};
	    download($linkelement->[$i], $r_level - 1, 0);
	}
    }
}

# get ImageProperties.xml and pull out the pertinent data
# while we're at it, compute a few things with that data.
sub get_ImageProperties {
    my $url = shift;

    $url = $url . "ImageProperties.xml";

    print "getting $url\n" if $optargs{debugging};
    my $response = $browser->get($url);
    die "Can't get $url -- ", $response->status_line
	unless $response->is_success;

    # there should only be a single image_properties xml element.  but we should do what we can 
    # to prevent bad things from future or bad inputs.  We are not going to bother with a full 
    # xml parser though.
    die "$url has no image properties" unless 
	$response->content =~ /\<\s*image_properties(\s.*?)\>/i;
    my $content = $1;

    my %ip;

    die "no width in $url" unless
	$content =~ /\swidth\=\"(\d+)\"/i;
    $ip{width} = $1;
    die "no height in $url" unless
	$content =~ /\sheight=\"(\d+)\"/i;
    $ip{height} = $1;
    die "no numtiles in $url" unless
	$content =~ /\snumtiles=\"(\d+)\"/i;
    $ip{numtiles} = $1;
    if ($content =~ /\snumimages=\"(\d+)\"/i) {
	$ip{numimages} = $1;
    } else {
	warn "no numimages in $url";
	$ip{numimages} = 1;
    }
    # BUG_MORE_THAN_ONE_IMAGE
    # This doesn't handle more than one image per zoominfo directory
    if ($ip{numimages} > 1) {
	warn "This zoomified url contains more than one image.  I can only process the first";
    }
    if ($content =~ /\sversion=\"(.+?)\"/i) {
	$ip{version} = $1;
    } else {
	warn "no version in $url";
	$ip{version} = "0.0";
    }
    die "no tilesize in $url" unless
	$content =~ /\stilesize=\"(\d+)\"/i;
    $ip{tilesize} = $1;

	

    my $w = $ip{width};
    my $h = $ip{height};
    
    my $m = 1;  # multiple 
    my $l = 0;  # zoom level
    while (($m * 256 < $w) || ($m * 256 < $h)) {
	$m *= 2;
	$l++;
    }

    my @zoomlevel;
    my $tiles = 0;

    for (my $curl = 0; $curl <= $l; ++$curl) {
	my $tw = ceil($w / ($m * 256));
	my $th = ceil($h / ($m * 256));
	my $pw = ceil($w / $m);
	my $ph = ceil($h / $m);
	$zoomlevel[$curl] = [$tw, $th, $tiles, $pw, $ph];
	$tiles += $tw * $th;
	$m /= 2;
    }
    
    $ip{zoomlevel} = \@zoomlevel;
    $ip{zlmax} = $l;
    
    return \%ip;
}

# tidy up the url.  Currently just make sure there's a trailing '/'
sub clean_url {
    my $url = shift;

    unless ($url =~/\/$/) {
	$url = $url . '/';
    }

    return $url;
}

# generate the full url for the specific tile
sub tile_url {
    my ($ip, $url, $level, $x, $y) = @_;

    my $imagenum = $ip->{zoomlevel}[$level][2] + $y * $ip->{zoomlevel}[$level][0] + $x;

    # BUG_TILEGROUP_SIZE
    # some sources say there are tilesize images per tilegroup, others just say 256... 
    # I don't know which is right.  This should be relatively simple to investigate though.

    my $tilegroup = floor($imagenum / $ip->{tilesize});
    my $fullurl = $url . "TileGroup" . $tilegroup . "/$level-$x-$y.jpg";
    return $fullurl;
}

# download a zoomified image
sub zoomify_dl {
    my $url = shift;
    $url = clean_url($url);

    # BUG_WONT_DOWNLOAD_TWICE
    # this won't download the same image twice.  Normally this is a feature, but
    # someone might want to download different sections of the same image.
    return if exists ($images_dled{"$optargs{zoomify_level} $url"});
    $images_dled{"$optargs{zoomify_level} $url"} = 1;

    print "processing $url\n" unless $optargs{quiet};

    my $fn_base;
    if ($optargs{use_alt_name}) {
	$fn_base = $optargs{alt_name};
    } else {
	unless ($url =~ '/([^/]+)/$') {
	    warn "Can't create filename base from url $url"; 
	    return;
	}
	$fn_base = $1;
    }

    my $ip = get_ImageProperties($url);


    if ($optargs{display_properties}) {
	print("width: $ip->{width}\nheight: $ip->{height}\nnumtiles: $ip->{numtiles}\n");
	print("numimages: $ip->{numimages}\nversion: $ip->{version}\ntilesize: $ip->{tilesize}\n");
	print "zlmax: $ip->{zlmax}\n";
	print "dimensions zoomlevel: (pixels width, height, tiles width, height)\n";
	for (my $a = 0; $a <= $ip->{zlmax}; ++$a) {
	    print "   $a: ($ip->{zoomlevel}[$a][3], $ip->{zoomlevel}[$a][4], $ip->{zoomlevel}[$a][0], $ip->{zoomlevel}[$a][1])\n";
	}
    }

    return if $optargs{no_download};

    if ((!$optargs{savetiles}) && ($optargs{noimage})) {
	warn "*** Cowardly refusing to download tiles unless they're being used.***\n";
	return;
    }

    my $level = $optargs{zoomify_level};
    $level = $ip->{zlmax} if $level eq 'max';

    if ($level > $ip->{zlmax}) {
	warn "Requested zoomlevel $level is invalid for $url\n";
	return;
    }

    my $start_x_tile = 0;
    my $end_x_tile = $ip->{zoomlevel}[$level][0];
    my $start_x = 0;
    my $width = $ip->{zoomlevel}[$level][3];
    my $start_y_tile = 0;
    my $end_y_tile = $ip->{zoomlevel}[$level][1];
    my $start_y = 0;
    my $height = $ip->{zoomlevel}[$level][4];
    if ($optargs{use_alt_geometry}) {
	my $ts = $ip->{tilesize};
	
	unless($optargs{alt_geometry} =~ /(?<width>         \d+\.?\d*)(?<width_percent>   \%?)
                                          x
                                          (?<height>        \d+\.?\d*)(?<height_percent>  \%?)
                                          \+
                                          (?<startx>        \d+\.?\d*)(?<startx_percent>  \%?)
                                          \+
                                          (?<starty>        \d+\.?\d*)(?<starty_percent>  \%?)/x) {
	    warn "Can't parse requested geometry for $url";
	    return;
	}
	
	$start_x = $+{startx};
	$start_x = $start_x / 100 * $ip->{zoomlevel}[$level][3] if $+{startx_percent} eq "%";
	$start_x = floor($start_x);
	$start_x_tile = floor($start_x / $ip->{tilesize});

	$width = $+{width};
	$width = $width / 100 * $ip->{zoomlevel}[$level][3] if $+{width_percent} eq "%";
	$width = floor($width);
	$end_x_tile = ceil(($start_x + $width) / $ip->{tilesize});
	
	$start_y = $+{starty};
	$start_y = $start_y / 100 * $ip->{zoomlevel}[$level][4] if $+{starty_percent} eq "%";
	$start_y = floor($start_y);
	$start_y_tile = floor($start_y / $ip->{tilesize});

	$height = $+{height};
	$height = $height / 100 * $ip->{zoomlevel}[$level][4] if $+{height_percent} eq "%";
	$height = floor($height);
	$end_y_tile = ceil(($start_y + $height) / $ip->{tilesize});
    }
    if ($optargs{debugging}) {
	print "start_x: $start_x, start_x_tile: $start_x_tile, width: $width, end_x_tile: $end_x_tile\n";
	print "start_y: $start_y, start_y_tile: $start_y_tile, height: $height, end_y_tile: $end_y_tile\n";
    }

    # BUG_NO_ALPHA_CHANNEL
    # We don't need the alpha channel.  Does imagemagick always allocate one?
    # I think there's a setting to get rid of it and realize a 25% memory savings

    my $r;  # errors / warnings from IM
    my $imagesize = $width . "x" . $height;
    my $image;
    unless ($optargs{noimage}) {
	$image=Image::Magick->new();
	if ($optargs{memory_limit}) {
	    $image->set('memory-limit' => $optargs{memory_limit});
	    $image->set('map-limit' => $optargs{memory_limit});
	}
	$image->Set('size' => $imagesize);
	$r = $image->ReadImage('xc:white');
	if ($r) {
	    warn "failed to initialize image with error \"$r\" for $url";
	    undef $image;
	    return;
	}
    }


    for (my $y = $start_y_tile; $y < $end_y_tile; ++$y) {
	for (my $x = $start_x_tile; $x < $end_x_tile; ++$x) {

	    # BUG_ADD_RETRY_LOGIC
	    # should add retry logic (and associated options) when we
	    # can't get a tile for some reason

	    my $turl = tile_url($ip, $url, $level, $x, $y);
	    print "$turl\n" unless $optargs{quiet};
	    my $response = $browser->get($turl);
	    unless ($response->is_success) {
		warn "Can't get $turl -- ", $response->status_line;
		return;
	    }
	    unless ($optargs{noimage}) {
		my $tile = Image::Magick->new(magick=>'jpg');
		$r = $tile->BlobToImage($response->content);
		warn "failed to parse jpeg tile ($r) $turl" if "$r";
		my $tile_geometry = "+" . ($x * $ip->{tilesize} - $start_x) . "+" . ($y * $ip->{tilesize} - $start_y);
		$tile_geometry =~ s/\+\-/\-/g;
		$r = $image->Composite(image => $tile,
				       geometry => $tile_geometry);
		warn "faid to place tile ($r) $turl" if "$r";
		undef $tile;
	    }
	    if ($optargs{savetiles}) {
		my $tilename = "$fn_base-$level-$x-$y.jpg";
		open (OUTFILE, ">$tilename") or warn "Can't create file $tilename";
		print OUTFILE $response->content;
		close (OUTFILE) or warn "Can't flush file $tilename";
	    }
	}
    }

    # BUG_ADD_JPEG_QUALITY
    # add an option to save at a different jpeg quality/compression level.

    # BUG_ALLOW_OTHER_FILE_TYPES
    # allow saving as something other than a jpeg.
    unless ($optargs{noimage}) {
	my $name = $fn_base . ".jpg";
	$r = $image->Write($name);
	warn "failed to write image to $name ($r) for $url" if "$r";
	undef $image;
    }
}
