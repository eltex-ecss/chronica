%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_format).

-export([
         default/1,
         binary/1,
         month_str_binary/1,
         get_priority_short_prefix/1,
         get_priority_prefix_up/1,
         get_priority_prefix_low/1
        ]).

-include_lib("pt_scripts/include/pt_recompilable.hrl").
-include("chronica_int.hrl").

default({{{Year, Month, Day}, {Hour, Minute, Second}, Millisecond},
         PriorityInt, Module, Pid, Line, _File, Function, UserStr}) ->
    Y_binary = year_binary(Year),
    Mo_binary = time(Month),
    D_binary = time(Day),
    H_binary = time(Hour),
    Minute_binary = time(Minute),
    Second_binary = time(Second),
    MilliSecond_binary = msecond_binary(Millisecond),
    Priority_cap_binary = get_priority_prefix_up(PriorityInt),
    Pid_binary = erlang:list_to_binary(erlang:pid_to_list(Pid)),
    Module_binary = erlang:atom_to_binary(Module, latin1),
    Function_binary = erlang:list_to_binary(Function),
    L_binary = erlang:integer_to_binary(Line),
    Message_binary = unicode:characters_to_binary(UserStr),

    <<Y_binary/binary, "-", Mo_binary/binary, "-",
      D_binary/binary, " ", H_binary/binary, ":", Minute_binary/binary, ":",
      Second_binary/binary, ".", MilliSecond_binary/binary, " ",
      Priority_cap_binary/binary, " ", Pid_binary/binary, " ",
        "[",
            Module_binary/binary, ":", Function_binary/binary,
            ":", L_binary/binary, "]",
    ":",  " ", Message_binary/binary, "\n" >>.

binary(Msg) ->
    term_to_binary(Msg).

get_priority_short_prefix(1) -> <<"*** ERROR">>;
get_priority_short_prefix(2) -> <<"W">>;
get_priority_short_prefix(3) -> <<"I">>;
get_priority_short_prefix(4) -> <<"T">>;
get_priority_short_prefix(5) -> <<"D">>.

get_priority_prefix_up(1) -> <<"ERROR">>;
get_priority_prefix_up(2) -> <<"WARN ">>;
get_priority_prefix_up(3) -> <<"INFO ">>;
get_priority_prefix_up(4) -> <<"TRACE">>;
get_priority_prefix_up(5) -> <<"DEBUG">>.

get_priority_prefix_low(1) -> <<"error">>;
get_priority_prefix_low(2) -> <<"warning">>;
get_priority_prefix_low(3) -> <<"info">>;
get_priority_prefix_low(4) -> <<"trace">>;
get_priority_prefix_low(5) -> <<"debug">>.

msecond_binary(N) when N < 10 ->
    <<"00000", (erlang:integer_to_binary(N))/binary>>;
msecond_binary(N) when N < 100 ->
    <<"0000", (erlang:integer_to_binary(N))/binary>>;
msecond_binary(N) when N < 1000 ->
    <<"000", (erlang:integer_to_binary(N))/binary>>;
msecond_binary(N) when N < 10000 ->
    <<"00", (erlang:integer_to_binary(N))/binary>>;
msecond_binary(N) when N < 100000 ->
    <<"0", (erlang:integer_to_binary(N))/binary>>;
msecond_binary(N) -> erlang:integer_to_binary(N).

time(0)  -> <<"00">>;
time(1)  -> <<"01">>;
time(2)  -> <<"02">>;
time(3)  -> <<"03">>;
time(4)  -> <<"04">>;
time(5)  -> <<"05">>;
time(6)  -> <<"06">>;
time(7)  -> <<"07">>;
time(8)  -> <<"08">>;
time(9)  -> <<"09">>;
time(10) -> <<"10">>;
time(11) -> <<"11">>;
time(12) -> <<"12">>;
time(13) -> <<"13">>;
time(14) -> <<"14">>;
time(15) -> <<"15">>;
time(16) -> <<"16">>;
time(17) -> <<"17">>;
time(18) -> <<"18">>;
time(19) -> <<"19">>;
time(20) -> <<"20">>;
time(21) -> <<"21">>;
time(22) -> <<"22">>;
time(23) -> <<"23">>;
time(24) -> <<"24">>;
time(25) -> <<"25">>;
time(26) -> <<"26">>;
time(27) -> <<"27">>;
time(28) -> <<"28">>;
time(29) -> <<"29">>;
time(30) -> <<"30">>;
time(31) -> <<"31">>;
time(32) -> <<"32">>;
time(33) -> <<"33">>;
time(34) -> <<"34">>;
time(35) -> <<"35">>;
time(36) -> <<"36">>;
time(37) -> <<"37">>;
time(38) -> <<"38">>;
time(39) -> <<"39">>;
time(40) -> <<"40">>;
time(41) -> <<"41">>;
time(42) -> <<"42">>;
time(43) -> <<"43">>;
time(44) -> <<"44">>;
time(45) -> <<"45">>;
time(46) -> <<"46">>;
time(47) -> <<"47">>;
time(48) -> <<"48">>;
time(49) -> <<"49">>;
time(50) -> <<"50">>;
time(51) -> <<"51">>;
time(52) -> <<"52">>;
time(53) -> <<"53">>;
time(54) -> <<"54">>;
time(55) -> <<"55">>;
time(56) -> <<"56">>;
time(57) -> <<"57">>;
time(58) -> <<"58">>;
time(59) -> <<"59">>;
time(N) -> erlang:list_to_binary(erlang:integer_to_list(N)).

month_str_binary(1)  -> <<"Jan">>;
month_str_binary(2)  -> <<"Feb">>;
month_str_binary(3)  -> <<"Mar">>;
month_str_binary(4)  -> <<"Apr">>;
month_str_binary(5)  -> <<"May">>;
month_str_binary(6)  -> <<"Jun">>;
month_str_binary(7)  -> <<"Jul">>;
month_str_binary(8)  -> <<"Aug">>;
month_str_binary(9)  -> <<"Sep">>;
month_str_binary(10) -> <<"Oct">>;
month_str_binary(11) -> <<"Nov">>;
month_str_binary(12) -> <<"Dec">>.

year_binary(2015) -> <<"2015">>;
year_binary(2016) -> <<"2016">>;
year_binary(2017) -> <<"2017">>;
year_binary(2018) -> <<"2018">>;
year_binary(2019) -> <<"2019">>;
year_binary(2020) -> <<"2020">>;
year_binary(2021) -> <<"2021">>;
year_binary(2022) -> <<"2022">>;
year_binary(2023) -> <<"2023">>;
year_binary(2024) -> <<"2024">>;
year_binary(2025) -> <<"2025">>;
year_binary(2026) -> <<"2026">>;
year_binary(2027) -> <<"2027">>;
year_binary(2028) -> <<"2028">>;
year_binary(2029) -> <<"2029">>;
year_binary(2030) -> <<"2030">>;
year_binary(2031) -> <<"2031">>;
year_binary(2032) -> <<"2032">>;
year_binary(2033) -> <<"2033">>;
year_binary(2034) -> <<"2034">>;
year_binary(2035) -> <<"2035">>;
year_binary(2036) -> <<"2036">>;
year_binary(2037) -> <<"2037">>;
year_binary(2038) -> <<"2038">>;
year_binary(2039) -> <<"2039">>;
year_binary(2040) -> <<"2040">>;
year_binary(2041) -> <<"2041">>;
year_binary(2042) -> <<"2042">>;
year_binary(2043) -> <<"2043">>;
year_binary(2044) -> <<"2044">>;
year_binary(2045) -> <<"2045">>;
year_binary(2046) -> <<"2046">>;
year_binary(2047) -> <<"2047">>;
year_binary(2048) -> <<"2048">>;
year_binary(2049) -> <<"2049">>;
year_binary(2050) -> <<"2050">>;
year_binary(2051) -> <<"2051">>;
year_binary(2052) -> <<"2052">>;
year_binary(2053) -> <<"2053">>;
year_binary(2054) -> <<"2054">>;
year_binary(2055) -> <<"2055">>;
year_binary(2056) -> <<"2056">>;
year_binary(2057) -> <<"2057">>;
year_binary(2058) -> <<"2058">>;
year_binary(2059) -> <<"2059">>;
year_binary(2060) -> <<"2060">>;
year_binary(2061) -> <<"2061">>;
year_binary(2062) -> <<"2062">>;
year_binary(2063) -> <<"2063">>;
year_binary(2064) -> <<"2064">>;
year_binary(2065) -> <<"2065">>;
year_binary(2066) -> <<"2066">>;
year_binary(2067) -> <<"2067">>;
year_binary(2068) -> <<"2068">>;
year_binary(2069) -> <<"2069">>;
year_binary(2070) -> <<"2070">>;
year_binary(2071) -> <<"2071">>;
year_binary(2072) -> <<"2072">>;
year_binary(2073) -> <<"2073">>;
year_binary(2074) -> <<"2074">>;
year_binary(2075) -> <<"2075">>;
year_binary(2076) -> <<"2076">>;
year_binary(2077) -> <<"2077">>;
year_binary(2078) -> <<"2078">>;
year_binary(2079) -> <<"2079">>;
year_binary(2080) -> <<"2080">>;
year_binary(2081) -> <<"2081">>;
year_binary(2082) -> <<"2082">>;
year_binary(2083) -> <<"2083">>;
year_binary(2084) -> <<"2084">>;
year_binary(2085) -> <<"2085">>;
year_binary(2086) -> <<"2086">>;
year_binary(2087) -> <<"2087">>;
year_binary(2088) -> <<"2088">>;
year_binary(2089) -> <<"2089">>;
year_binary(2090) -> <<"2090">>;
year_binary(2091) -> <<"2091">>;
year_binary(2092) -> <<"2092">>;
year_binary(2093) -> <<"2093">>;
year_binary(2094) -> <<"2094">>;
year_binary(2095) -> <<"2095">>;
year_binary(2096) -> <<"2096">>;
year_binary(2097) -> <<"2097">>;
year_binary(2098) -> <<"2098">>;
year_binary(2099) -> <<"2099">>;
year_binary(2100) -> <<"2100">>;
year_binary(N) -> erlang:list_to_binary(erlang:integer_to_list(N)).
