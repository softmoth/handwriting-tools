#! /usr/bin/env perl

# Copyright 2018 Tim Smith <tim.dolores@gmail.com>

# Based on joiner.jar, made by Jonathan Harris for the italic handwriting
# project and distributed under the GNU Public License, version 3.
# See http://briem.net/4/4201/4201_4211.html (as of March 2018)
#
# While intended to be released under the GPL, I don't find the source.
# This Perl program does not use any code from joiner.jar, only the basic
# concept and the data from data/JoinList.txt for the Briem Handwriting
# font.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


use 5.014;
use autodie;

use Encode qw/find_encoding/;
use Encode::Guess;
use Getopt::Long;

# Send messages to terminal as UTF-8
binmode(STDOUT, ":encoding(UTF-8)");
binmode(STDERR, ":encoding(UTF-8)");

my (%options, %joins, $join_re, $errors);

init_options();
init_joins();

foreach my $file (@ARGV) {
    my ($text, $decoder);

    $text = slurp_text($file);
    ($decoder, $text) = decode_text($file, $text);

    # This is it! This inserts the correct join character (or
    # nothing, if there is no join character defined for the
    # letter pair) between every two pairs of letters.
    $text =~ s{
        ($join_re)      # Match a character, $1
        \K              # Set cursor to *after* that character
        (?=($join_re))  # Look ahead for another character, $2
    }{
        # If a join is defined for this letter pair, use it
        $joins{"$1$2"} ||  ''
    }egx;

    spurt_text($file, $decoder, $text);
}

exit($errors > 0 ? 2 : 0);


sub init_options {
    Getopt::Long::Configure('bundling');

    GetOptions(
        'help|?|h!'   => \(my $help = 0),
        'manual|man!' => \(my $manual = 0),
        'joins=s'     => \($options{joins}),
        'decode=s'    => \($options{decode} = 'latin1'),
        'encode=s'    => \($options{encode}),
        'force!'  => \($options{force} = 0),
    )
        or die usage();

    if ($help) {
        print usage();
        exit 1;
    }

    if ($manual) {
        print manual();
        exit;
    }

    if (@ARGV == 0) {
        # Get text from standard input
        unshift @ARGV, '-';
    }

    die usage("$options{decode} is not a valid Encoding name")
        unless find_encoding($options{decode});

    if ($options{encode}) {
        die usage("$options{encode} is not a valid Encoding name")
            unless find_encoding($options{encode});
        # XXX Is there a better test for this?
        die usage("--encode=$options{encode} must be a Unicode encoding")
            unless $options{encode} =~ /^utf/i;
    }

    foreach (@ARGV) {
        next if $_ eq '-';

        die "Can't read file '$_'\n" unless -r $_;
        my $out = output_name($_);
        die usage("Refusing to overwrite '$out'")
            if !$options{force} and -e $out;
    }

    1;
}

sub usage {
    my ($message) = @_;
    (my $prog = $0) =~ s{^.*/}//;
    $message = '' unless $message;
    $message = "$message\n" if $message;

    return <<EOF;
${message}Usage: $prog [OPTIONS] [FILE ...]

If no FILE is given, or FILE is "-", text will be read from standard input
and written to standard output.


OPTIONS

  --decode=ENCODING  Assume input is in ENCODING (e.g. 'latin-2')
  --encode=ENCODING  Write output in ENCODING (e.g. 'utf-8')
  --joins=FILE       Read joins data from FILE (defaults to built-in data)
  --force            Overwrite an existing FOO-joined.txt output file

  -? | -h | --help   Show this help message
  --man | --manual   Show extended help information
EOF
}

sub manual {
    (my $prog = $0) =~ s{^.*/}//;
    return usage() . <<EOF;


EXAMPLES

# Using standard input and standard output
$prog < Examples/Beowulf.txt > Examples/Beowulf-joined.txt

# This is the same as the previous example, but will refuse to write
# over "Beowulf-joined.txt" if it already exists
$prog Examples/Beowulf.txt

# This will create "Examples/FOO-joined.txt" for each Example text
$prog Examples/*.txt


CHARACTER ENCODINGS

Decoding always tries UTF-8/16/32 and ASCII first. If none of those
is successful, then --decode=NAME or latin1 will be tried.

If no --encode option is given, the output will use the same encoding as
the input if it is a Unicode encoding. Otherwise, UTF-8 will be used.

For a list of supported encoding names:
https://metacpan.org/pod/distribution/Encode/lib/Encode/Supported.pod


JOINS DATA FILE

By default, a built-in data file (starting after the word __DATA__ at
the end of this file) is used. These joins work with the Briem Handwriting
font available from briem.net. An external data file can be read, to
accommodate a different font.

The data file is a list of character triplets. Triplet "AXB" defines a letter
pair "AB" which is joined by "X". Whenever "A" is followed by "B" in the input
text, "X" will be inserted between them on output.

Triplets are separated by white space (any of " \t\r\n").

Lines beginning with "#" are ignored as comments.

The data file must use a UTF-8/-16/32 encoding.
EOF

}


sub init_joins {
    my $fh;
    if ($options{joins}) {
        open $fh, '<', $options{joins};
    }
    else {
        $fh = \*DATA;
    }
    my $data = do { local $/ = undef; <$fh> };

    my $decoder = guess_encoding($data);
    die "ERROR: joins data does not look like UTF: $decoder\n"
        unless ref($decoder);

    my $joins = $decoder->decode($data);

    # Strip comments and trim ends
    $joins =~ s/^^#[^\r\n]*[\r\n]+//mg;
    $joins =~ s/^\s+//s;
    $joins =~ s/\s+$//s;

    foreach (split /[ \t\r\n]+/, $joins) {
        warn "Bad join triplet '$_' (",
            map({ sprintf "x[%4x]", $_ } split ''), ") at DATA line $."
            unless length($_) == 3;

        my ($a, $join, $b) = split '';
        warn "Repeated letter pair '$a$b' at DATA line $."
            if exists $joins{"$a$b"};
        $joins{"$a$b"} = $join;
    }

    # Now that %joins is initialized, create a regex snippet that
    # will match a single joinable character (a character that
    # can be part of a joined letter pair)
    my %cclass;
    foreach (keys %joins) {
        ++$cclass{$_} foreach split '';
    }
    my $cclass = join '',
        map { sprintf '\x{%x}', ord($_) }
        sort keys %cclass;
    $cclass = "[$cclass]";
    $join_re = qr/$cclass/;

    1;
}


sub slurp_text {
    my ($file) = @_;

    my $in;
    if ($file eq '-') {
        $in = \*STDIN;
    }
    else {
        open $in, '<', $file;
    }

    my $text = do { local $/ = undef; <$in> };
    close $in unless $file eq '-';

    return $text;
}


sub decode_text {
    my ($file, $text) = @_;

    # Check for ASCII, UTF-8/16/32 first
    my $decoder = guess_encoding($text);

    if (!ref($decoder)) {
        $decoder = find_encoding($options{decode});
    }

    #warn sprintf("Reading text as %s (%s)\n",
    #    $decoder->name, $decoder->mime_name);
    $text = $decoder->decode($text);

    return ($decoder, $text);
}


sub spurt_text {
    my ($file, $decoder, $text) = @_;

    my $encoding;
    if ($options{encode}) {
        $encoding = $options{encode};
    }
    elsif ($decoder->name =~ /^utf/i) {
        # Preserve file encoding
        $encoding = $decoder->name;
    }
    else {
        $encoding = 'UTF-8';
    }

    my $out;
    if ($file eq '-') {
        $out = \*STDOUT;
    }
    else {
        my $outname = output_name($file);
        open $out, '>', $outname;
    }
    binmode($out, ":encoding($encoding)");

    print {$out} $text;
}

sub output_name {
    my ($input_name) = @_;
    my $ext = rindex($input_name, '.');
    $ext = length($input_name) if $ext < 0;
    substr($input_name, $ext, 0, '-joined');
    return $input_name;
}


__DATA__

# This join list is for the Briem Handwriting typeface
# See http://briem.net/4/4201/4201_4051.html (as of March 2018)

aa aá aà aâ aä aå aã ad ag aq aæ ab af ah ak al at aþ ai aí aì aî aï am an añ ap ar au aú aù aû aü aj ay aý aÿ ac aç ae aé aè aê aë ao aò aó aô aö aõ aø aœ að as av aw ax az
áa áá áà áâ áä áå áã ád ág áq áæ áb áf áh ák ál át áþ ái áí áì áî áï ám án áñ áp ár áu áú áù áû áü áj áy áý áÿ ác áç áe áé áè áê áë áo áò áó áô áö áõ áø áœ áð ás áv áw áx áz
àa àá àà àâ àä àå àã àd àg àq àæ àb àf àh àk àl àt àþ ài àí àì àî àï àm àn àñ àp àr àu àú àù àû àü àj ày àý àÿ àc àç àe àé àè àê àë ào àò àó àô àö àõ àø àœ àð às àv àw àx àz
âa âá âà ââ âä âå âã âd âg âq âæ âb âf âh âk âl ât âþ âi âí âì âî âï âm ân âñ âp âr âu âú âù âû âü âj ây âý âÿ âc âç âe âé âè âê âë âo âò âó âô âö âõ âø âœ âð âs âv âw âx âz
äa äá äà äâ ää äå äã äd äg äq äæ äb äf äh äk äl ät äþ äi äí äì äî äï äm än äñ äp är äu äú äù äû äü äj äy äý äÿ äc äç äe äé äè äê äë äo äò äó äô äö äõ äø äœ äð äs äv äw äx äz
åa åá åà åâ åä åå åã åd åg åq åæ åb åf åh åk ål åt åþ åi åí åì åî åï åm ån åñ åp år åu åú åù åû åü åj åy åý åÿ åc åç åe åé åè åê åë åo åò åó åô åö åõ åø åœ åð ås åv åw åx åz
ãa ãá ãà ãâ ãä ãå ãã ãd ãg ãq ãæ ãb ãf ãh ãk ãl ãt ãþ ãi ãí ãì ãî ãï ãm ãn ãñ ãp ãr ãu ãú ãù ãû ãü ãj ãy ãý ãÿ ãc ãç ãe ãé ãè ãê ãë ão ãò ãó ãô ãö ãõ ãø ãœ ãð ãs ãv ãw ãx ãz
da dá dà dâ dä då dã dd dg dq dæ db df dh dk dl dt dþ di dí dì dî dï dm dn dñ dp dr du dú dù dû dü dj dy dý dÿ dc dç de dé dè dê dë do dò dó dô dö dõ dø dœ dð ds dv dw dx dz
ha há hà hâ hä hå hã hd hg hq hæ hb hf hh hk hl ht hþ hi hí hì hî hï hm hn hñ hp hr hu hú hù hû hü hj hy hý hÿ hc hç he hé hè hê hë ho hò hó hô hö hõ hø hœ hð hs hv hw hx hz
ia iá ià iâ iä iå iã id ig iq iæ ib if ih ik il it iþ ii ií iì iî iï im in iñ ip ir iu iú iù iû iü ij iy iý iÿ ic iç ie ié iè iê ië io iò ió iô iö iõ iø iœ ið is iv iw ix iz
ía íá íà íâ íä íå íã íd íg íq íæ íb íf íh ík íl ít íþ íi íí íì íî íï ím ín íñ íp ír íu íú íù íû íü íj íy íý íÿ íc íç íe íé íè íê íë ío íò íó íô íö íõ íø íœ íð ís ív íw íx íz
ìa ìá ìà ìâ ìä ìå ìã ìd ìg ìq ìæ ìb ìf ìh ìk ìl ìt ìþ ìi ìí ìì ìî ìï ìm ìn ìñ ìp ìr ìu ìú ìù ìû ìü ìj ìy ìý ìÿ ìc ìç ìe ìé ìè ìê ìë ìo ìò ìó ìô ìö ìõ ìø ìœ ìð ìs ìv ìw ìx ìz
îa îá îà îâ îä îå îã îd îg îq îæ îb îf îh îk îl ît îþ îi îí îì îî îï îm în îñ îp îr îu îú îù îû îü îj îy îý îÿ îc îç îe îé îè îê îë îo îò îó îô îö îõ îø îœ îð îs îv îw îx îz
ïa ïá ïà ïâ ïä ïå ïã ïd ïg ïq ïæ ïb ïf ïh ïk ïl ït ïþ ïi ïí ïì ïî ïï ïm ïn ïñ ïp ïr ïu ïú ïù ïû ïü ïj ïy ïý ïÿ ïc ïç ïe ïé ïè ïê ïë ïo ïò ïó ïô ïö ïõ ïø ïœ ïð ïs ïv ïw ïx ïz
la lá là lâ lä lå lã ld lg lq læ lb lf lh lk ll lt lþ li lí lì lî lï lm ln lñ lp lr lu lú lù lû lü lj ly lý lÿ lc lç le lé lè lê lë lo lò ló lô lö lõ lø lœ lð ls lv lw lx lz
ma má mà mâ mä må mã md mg mq mæ mb mf mh mk ml mt mþ mi mí mì mî mï mm mn mñ mp mr mu mú mù mû mü mj my mý mÿ mc mç me mé mè mê më mo mò mó mô mö mõ mø mœ mð ms mv mw mx mz
na ná nà nâ nä nå nã nd ng nq næ nb nf nh nk nl nt nþ ni ní nì nî nï nm nn nñ np nr nu nú nù nû nü nj ny ný nÿ nc nç ne né nè nê në no nò nó nô nö nõ nø nœ nð ns nv nw nx nz
ña ñá ñà ñâ ñä ñå ñã ñd ñg ñq ñæ ñb ñf ñh ñk ñl ñt ñþ ñi ñí ñì ñî ñï ñm ñn ññ ñp ñr ñu ñú ñù ñû ñü ñj ñy ñý ñÿ ñc ñç ñe ñé ñè ñê ñë ño ñò ñó ñô ñö ñõ ñø ñœ ñð ñs ñv ñw ñx ñz
ua uá uà uâ uä uå uã ud ug uq uæ ub uf uh uk ul ut uþ ui uí uì uî uï um un uñ up ur uu uú uù uû uü uj uy uý uÿ uc uç ue ué uè uê uë uo uò uó uô uö uõ uø uœ uð us uv uw ux uz
úa úá úà úâ úä úå úã úd úg úq úæ úb úf úh úk úl út úþ úi úí úì úî úï úm ún úñ úp úr úu úú úù úû úü új úy úý úÿ úc úç úe úé úè úê úë úo úò úó úô úö úõ úø úœ úð ús úv úw úx úz
ùa ùá ùà ùâ ùä ùå ùã ùd ùg ùq ùæ ùb ùf ùh ùk ùl ùt ùþ ùi ùí ùì ùî ùï ùm ùn ùñ ùp ùr ùu ùú ùù ùû ùü ùj ùy ùý ùÿ ùc ùç ùe ùé ùè ùê ùë ùo ùò ùó ùô ùö ùõ ùø ùœ ùð ùs ùv ùw ùx ùz
ûa ûá ûà ûâ ûä ûå ûã ûd ûg ûq ûæ ûb ûf ûh ûk ûl ût ûþ ûi ûí ûì ûî ûï ûm ûn ûñ ûp ûr ûu ûú ûù ûû ûü ûj ûy ûý ûÿ ûc ûç ûe ûé ûè ûê ûë ûo ûò ûó ûô ûö ûõ ûø ûœ ûð ûs ûv ûw ûx ûz
üa üá üà üâ üä üå üã üd üg üq üæ üb üf üh ük ül üt üþ üi üí üì üî üï üm ün üñ üp ür üu üú üù üû üü üj üy üý üÿ üc üç üe üé üè üê üë üo üò üó üô üö üõ üø üœ üð üs üv üw üx üz
ba bá bà bâ bä bå bã bd bg bq bæ bb bf bh bk bl bt bþ bi bí bì bî bï bm bn bñ bp br bu bú bù bû bü bj by bý bÿ bc bç be bé bè bê bë bo bò bó bô bö bõ bø bœ bð bs bv bw bx bz
pa pá pà pâ pä på pã pd pg pq pæ pb pf ph pk pl pt pþ pi pí pì pî pï pm pn pñ pp pr pu pú pù pû pü pj py pý pÿ pc pç pe pé pè pê pë po pò pó pô pö põ pø pœ pð ps pv pw px pz
þa þá þà þâ þä þå þã þd þg þq þæ þb þf þh þk þl þt þþ þi þí þì þî þï þm þn þñ þp þr þu þú þù þû þü þj þy þý þÿ þc þç þe þé þè þê þë þo þò þó þô þö þõ þø þœ þð þs þv þw þx þz
ca cá cà câ cä cå cã cd cg cq cæ cb cf ch ck cl ct cþ ci cí cì cî cï cm cn cñ cp cr cu cú cù cû cü cj cy cý cÿ cc cç ce cé cè cê cë co cò có cô cö cõ cø cœ cð cs cv cw cx cz
ça çá çà çâ çä çå çã çd çg çq çæ çb çf çh çk çl çt çþ çi çí çì çî çï çm çn çñ çp çr çu çú çù çû çü çj çy çý çÿ çc çç çe çé çè çê çë ço çò çó çô çö çõ çø çœ çð çs çv çw çx çz
ea eá eà eâ eä eå eã ed eg eq eæ eb ef eh ek el et eþ ei eí eì eî eï em en eñ ep er eu eú eù eû eü ej ey eý eÿ ec eç ee eé eè eê eë eo eò eó eô eö eõ eø eœ eð es ev ew ex ez
éa éá éà éâ éä éå éã éd ég éq éæ éb éf éh ék él ét éþ éi éí éì éî éï ém én éñ ép ér éu éú éù éû éü éj éy éý éÿ éc éç ée éé éè éê éë éo éò éó éô éö éõ éø éœ éð és év éw éx éz
èa èá èà èâ èä èå èã èd èg èq èæ èb èf èh èk èl èt èþ èi èí èì èî èï èm èn èñ èp èr èu èú èù èû èü èj èy èý èÿ èc èç èe èé èè èê èë èo èò èó èô èö èõ èø èœ èð ès èv èw èx èz
êa êá êà êâ êä êå êã êd êg êq êæ êb êf êh êk êl êt êþ êi êí êì êî êï êm ên êñ êp êr êu êú êù êû êü êj êy êý êÿ êc êç êe êé êè êê êë êo êò êó êô êö êõ êø êœ êð ês êv êw êx êz
ëa ëá ëà ëâ ëä ëå ëã ëd ëg ëq ëæ ëb ëf ëh ëk ël ët ëþ ëi ëí ëì ëî ëï ëm ën ëñ ëp ër ëu ëú ëù ëû ëü ëj ëy ëý ëÿ ëc ëç ëe ëé ëè ëê ëë ëo ëò ëó ëô ëö ëõ ëø ëœ ëð ës ëv ëw ëx ëz
æa æá æà æâ æä æå æã æd æg æq ææ æb æf æh æk æl æt æþ æi æí æì æî æï æm æn æñ æp ær æu æú æù æû æü æj æy æý æÿ æc æç æe æé æè æê æë æo æò æó æô æö æõ æø æœ æð æs æv æw æx æz
œa œá œà œâ œä œå œã œd œg œq œæ œb œf œh œk œl œt œþ œi œí œì œî œï œm œn œñ œp œr œu œú œù œû œü œj œy œý œÿ œc œç œe œé œè œê œë œo œò œó œô œö œõ œø œœ œð œs œv œw œx œz
fa fá fà fâ fä få fã fd fg fq fæ fb ff fh fk fl ft fþ fi fí fì fî fï fm fn fñ fp fr fu fú fù fû fü fj fy fý fÿ fc fç fe fé fè fê fë fo fò fó fô fö fõ fø fœ fð fs
ta tá tà tâ tä tå tã td tg tq tæ tb tf th tk tl tt tþ ti tí tì tî tï tm tn tñ tp tr tu tú tù tû tü tj ty tý tÿ tc tç te té tè tê të to tò tó tô tö tõ tø tœ tð ts
ka ká kà kâ kä kå kã kd kg kq kæ kb kf kh kk kl kt kþ ki kí kì kî kï km kn kñ kp kr ku kú kù kû kü kj ky ký kÿ kc kç ke ké kè kê kë ko kò kó kô kö kõ kø kœ kð ks kv kw kx kz
oa oá oà oâ oä oå oã od og oq oæ ob of oh ok ol ot oþ oi oí oì oî oï om on oñ op or ou oú où oû oü oj oy oý oÿ oc oç oe oé oè oê oë oo oò oó oô oö oõ oø oœ oð os ov ow ox oz
òa òá òà òâ òä òå òã òd òg òq òæ òb òf òh òk òl òt òþ òi òí òì òî òï òm òn òñ òp òr òu òú òù òû òü òj òy òý òÿ òc òç òe òé òè òê òë òo òò òó òô òö òõ òø òœ òð òs òv òw òx òz
óa óá óà óâ óä óå óã ód óg óq óæ ób óf óh ók ól ót óþ ói óí óì óî óï óm ón óñ óp ór óu óú óù óû óü ój óy óý óÿ óc óç óe óé óè óê óë óo óò óó óô óö óõ óø óœ óð ós óv ów óx óz
ôa ôá ôà ôâ ôä ôå ôã ôd ôg ôq ôæ ôb ôf ôh ôk ôl ôt ôþ ôi ôí ôì ôî ôï ôm ôn ôñ ôp ôr ôu ôú ôù ôû ôü ôj ôy ôý ôÿ ôc ôç ôe ôé ôè ôê ôë ôo ôò ôó ôô ôö ôõ ôø ôœ ôð ôs ôv ôw ôx ôz
öa öá öà öâ öä öå öã öd ög öq öæ öb öf öh ök öl öt öþ öi öí öì öî öï öm ön öñ öp ör öu öú öù öû öü öj öy öý öÿ öc öç öe öé öè öê öë öo öò öó öô öö öõ öø öœ öð ös öv öw öx öz
õa õá õà õâ õä õå õã õd õg õq õæ õb õf õh õk õl õt õþ õi õí õì õî õï õm õn õñ õp õr õu õú õù õû õü õj õy õý õÿ õc õç õe õé õè õê õë õo õò õó õô õö õõ õø õœ õð õs õv õw õx õz
øa øá øà øâ øä øå øã ød øg øq øæ øb øf øh øk øl øt øþ øi øí øì øî øï øm øn øñ øp ør øu øú øù øû øü øj øy øý øÿ øc øç øe øé øè øê øë øo øò øó øô øö øõ øø øœ øð øs øv øw øx øz
ra rá rà râ rä rå rã rd rg rq ræ rb rf rh rk rl rt rþ ri rí rì rî rï rm rn rñ rp rr ru rú rù rû rü rj ry rý rÿ rc rç re ré rè rê rë ro rò ró rô rö rõ rø rœ rð rs rv rw rx rz
sa sá sà sâ sä så sã sd sg sq sæ sb sf sh sk sl st sþ si sí sì sî sï sm sn sñ sp sr su sú sù sû sü sj sy sý sÿ sc sç se sé sè sê së so sò só sô sö sõ sø sœ sð ss sv sw sx sz
va vá và vâ vä vå vã vd vg vq væ vb vf vh vk vl vt vþ vi ví vì vî vï vm vn vñ vp vr vu vú vù vû vü vj vy vý vÿ vc vç ve vé vè vê vë vo vò vó vô vö võ vø vœ vð vs vv vw vx vz
wa wá wà wâ wä wå wã wd wg wq wæ wb wf wh wk wl wt wþ wi wí wì wî wï wm wn wñ wp wr wu wú wù wû wü wj wy wý wÿ wc wç we wé wè wê wë wo wò wó wô wö wõ wø wœ wð ws wv ww wx wz
za zá zà zâ zä zå zã zd zg zq zæ zb zf zh zk zl zt zþ zi zí zì zî zï zm zn zñ zp zr zu zú zù zû zü zj zy zý zÿ zc zç ze zé zè zê zë zo zò zó zô zö zõ zø zœ zð zs zv zw zx zz
