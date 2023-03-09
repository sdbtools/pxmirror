#!/usr/bin/gprolog --consult-file
% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- initialization(main).

:- include('lib/atom_common.pl').
:- include('lib/list_common.pl').
:- include('lib/cli_common.pl').
:- include('lib/os_common.pl').
:- include('lib/unix_common.pl').
:- include('lib/tui_common.pl').
:- include('lib/tui_dialog.pl').

:- dynamic([m/4, setting/2]).

setting(root_dir, '').
% setting(no_fetch, true).

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.

mirror(list, LD) :-
	setting(root_dir, RD),
	atom_concat(RD, '/usr/share/xmirror/mirrors.lst', LD),
	true.
% mirror(list, 'mirrors.lst').
mirror(plist, LD) :-
	setting(root_dir, RD),
	atom_concat(RD, '/usr/share/xmirror/mirrors.pl', LD),
	true.
mirror(url, 'https://xmirror.voidlinux.org/raw/mirrors.lst').

setup_tui :-
	retractall(tui_def_args_all(_)),
	asserta(tui_def_args_all([sz(auto), clear, no-shadow])).

% Uses names and abbreviations from https://planetarynames.wr.usgs.gov/Abbreviations
region_name('AF', 'Africa').
region_name('AN', 'Antarctica').
region_name('AS', 'Asia').
region_name('EU', 'Europe').
region_name('NA', 'North America').
region_name('OC', 'Oceania').
region_name('SA', 'South and Central America').
region_name('World', 'Globally Available').

region_menu(R, [R, N]) :-
	region_name(R, N), !,
	true.

menu :-
	dialog_msg(menu, MENULABEL),
	findall(R, m(R, _Url, _Location, _N), RL),
	sort(RL, SRL),
	maplist(region_menu, SRL, ML1),
	append(ML1, [
			['Default', 'Reset to the default mirror'],
			['Exit', 'Exit this wizard']
		], ML2),
	repeat,
	tui_menu_tag2(choose_region, ML2, MENULABEL, [no-cancel, title(' Choose a Region ')], Tag),
	on_region(Tag),
	% writenl(Tag),
	true.

on_region('Exit') :- !,
	true.
on_region(R) :- !,
	setting(root_dir, RD),
	reset_mirrors(R, RD),
	fail.

% RD - root directory
reset_mirrors('Default', RD) :- !,
	get_files(FL, RD),
	( maplist(delete_file, FL)
	; tui_msgbox('Couldn\'t delete configuration files', []),
	  fail
	),
	xbps_install_sync(RD, default),
	!.
reset_mirrors(R, RD) :-
	findall([Location, N], m(R, _Url, Location, N), UL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(UL, MENULABEL, [title(' Location List ')], Tag),
	m(R, Url, Tag, _N),
	format_to_atom(RDD1, '"~w/etc/xbps.d"', [RD]),
	format_to_atom(RDD2, '"~w/etc/xbps.d/"', [RD]),
	format_to_atom(RDC, '"~w/usr/share/xbps.d"/*-repository-*.conf', [RD]),
	os_call2([mkdir, o(p), RDD1]),
	os_shell2([cp, oo('no-preserve', 'mode,ownership'), oo(backup, simple), oo(suffix, '.old'), RDC, RDD2]),
	atom_strip_end(Url, '/', SUrl),
	format_to_atom(SED1, '"s|https://repo-default.voidlinux.org|~w|g"', [SUrl]),
	format_to_atom(RDR, '"~w/etc/xbps.d"/*-repository-*.conf', [RD]),
	os_shell2([sed, o(i), SED1, RDR]),
	xbps_install_sync(RD, SUrl),
	!.
reset_mirrors(R, _) :-
	tui_msgbox2(['Couldn\'t reset mirror for', R], []),
	fail.

atom_strip_end(I, E, O) :-
	atom_concat(O, E, I), !,
	true.
atom_strip_end(I, _, I).

xbps_install_sync(RD, M) :-
	get_xbps_program(install, XP),
	C0 = ['>/dev/null'],
	( RD = '' ->
	  C1 = C0
	; C1 = [o(r, RD)| C0]
	),
	C = [XP, oo(sync)| C1],
	% ( tui_progressbox(C, '', [title(' Sync repo '), sz([6, 80])])
	% ( os_call2([XP, oo(sync), o(r, RD)])
	( os_shell2(C)
	; tui_msgbox2(['Couldn\'t reset mirror to', M], []),
	  fail
	), !,
	tui_msgbox2(['Mirror is successfully reset to', M], []),
	true.

get_files(FL, RD) :-
	format_to_atom(C, 'find "~w/usr/share/xbps.d/" -name \'*-repository-*.conf\' | sort -u | sed \'s|/usr/share|/etc|g\'', [RD]),
	os_shell_lines(C, FL),
	% tui_msgbox2(FL, []),
	true.

do_install :-
	ux_user_root, !,
	check_program(dialog),
	setup_tui,
	get_mirrors,
	menu,
	true.
do_install :-
	writenl('Application must run as root.'),
	fail.

check_program(P) :-
	os_shell2([which, P, '>/dev/null', '2>&1']), !,
	true.
check_program(P) :-
	atom_concat('Could not find ', P, M),
	writenl(M),
	fail.

get_xbps_program(P, XP) :-
	atom_concat('xbps-', P, PA),
	( os_shell2([which, PA, '>/dev/null', '2>&1']),
	  XP = PA
	; atom_concat(PA, '.static', PAS),
	  os_shell2([which, PAS, '>/dev/null', '2>&1']),
	  XP = PAS
	),
	!.
get_xbps_program(P, _) :-
	atom_concat('xbps-', P, PA),
	atom_concat(PA, '.static', PAS),
	tui_msgbox2(['Could not locate', PA, or, PAS], []),
	fail.

remove_comments([[0'#|_]|T], L) :- !,
	remove_comments(T, L).
remove_comments([H|T], [H|L]) :- !,
	remove_comments(T, L).
remove_comments([], []).

parse_mirror(L, m(R, Url, Location, N)) :-
	split_list_ne(L, "\t", L1),
	maplist(codes_atom, L1, [R, Url, Location, N]),
	true.

parse_mirrors(S, PML) :-
	read_file_codes_lines(S, CL),
	remove_comments(CL, ML),
	maplist(parse_mirror, ML, PML),
	true.

get_mirrors :-
	setting(no_fetch, true),
	mirror(list, ML),
	file_exists(ML),
	handle_mirrors(ML),
	!.
get_mirrors :-
	% download.
	get_xbps_program(fetch, XF),
	mirror(list, ML),
	mirror(url, MURL),
	decompose_file_name(ML, Dir, _, _),
	os_call2([mkdir, o(p), Dir]),
	os_shell2([XF, o(o), ML, MURL, '>/dev/null']),
	handle_mirrors(ML),
	true.

handle_mirrors(ML) :-
	% parse.
	open(ML, read, S),
	parse_mirrors(S, PML),
	close(S),
	% postprocess.
	sort(PML, SML),
	maplist(assertz, SML),
	true.

version :-
	writenl('version 0.1').

usage :-
	cmd_arg_usage_short(SAL),
	surround_atoms('[', ']', SAL, SAL1),
	join_atoms(['Usage:',pxmirror|SAL1], ' ', SAL2),
	writenl(SAL2), nl,
	writenl('Void Linux mirror selection implemented in GNU Prolog.'), nl,
	cmd_arg_usage_long(S),
	writenl(S), nl,
	writenl('Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license'),
	true.

% cmd_arg_info(alias, short, long, value_descr, descr)
% short argument is a character code.
cmd_arg_info(help, 0'h, help, '', 'Show this help and exit.').
cmd_arg_info(version, 0'v, version, '', 'Show the version and exit.').
cmd_arg_info(nofetch, 0'n, 'no-fetch', '', 'Don\'t attempt to update the mirror list.').
cmd_arg_info(rootdir, 0'r, rootdir, rootdir, 'Use an alternative rootdir. Acts like xbps\'s -r flag.').

% first argument is an alias.
on_cmd_arg(help, _, _) :- !,
	usage,
	fail.
on_cmd_arg(version, _, _) :- !,
	version,
	fail.
on_cmd_arg(nofetch, L, L) :- !,
	retractall(setting(no_fetch, _)),
	assertz(setting(no_fetch, true)),
	true.
on_cmd_arg(rootdir, LI, T) :- !,
	( LI = [RD|T]
	; writenl('rootdir value expected'),
	  fail
	), !,
	retractall(setting(root_dir, _)),
	assertz(setting(root_dir, RD)),
	true.

main :-
	argument_list(AL),
	( handle_cmd_args(AL) ->
	  do_install,
	  os_call2([clear])
	; true
	),
	halt.
main :-
	writenl('pxmirror has failed.'),
	halt.

