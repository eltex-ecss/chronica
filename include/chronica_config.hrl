%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-include("chronica_color.hrl").
-type chronica_flow_id() :: atom().
-type chronica_priority() :: debug | trace | info | warning | error | nothing.

-record(chronica_rule,
        {
            id       :: atom() | any(),
            mask     :: string() | any(), % use *, ?, |, !
            priority :: debug | trace | info | warning | error | nothing,
            flow_ids :: [chronica_flow_id()],
            in_work  :: true | false
        }).

-type chronica_tty_backend()  :: tty.
-type chronica_file_backend() :: {file, Filename :: string()}.
-type chronica_udp_backend()  :: {udp, {IP :: string(), Port :: integer()}}.
-type chronica_backend()      :: {Type :: atom(), Params :: term()}.

-type chronica_format_id() :: atom().

-record(chronica_backend,
        {
            type   :: chronica_tty_backend() | chronica_file_backend() | chronica_udp_backend() | chronica_backend() | any(),
            format :: chronica_format_id() | any()
        }).

-record(chronica_flow,
        {
            flow_id  :: chronica_flow_id() | any(),
            backends :: [#chronica_backend{}] | any()
        }).

-record(chronica_format,
        {
            format_id :: chronica_format_id(),
            %% Format macros:
            % %Y - years
            % %M - month (digit)
            % %Ml - month (abbrev)
            % %D - day
            % %H - hour
            % %Mi - minutes
            % %S - seconds
            % %Ms - miliseconds
            % %PRIORITY - log verbose level caps (WARNING, ERROR, etc)
            % %P - short log level (W, E)
            % %Priority - log verbose level no caps (info, error, ...)
            % %Pid - pid logging calls
            % %File %Module %Function %Line - place logging calls
            % %Message
            format    :: string()
        }).

-type chronica_name_atom_coloring() :: 'Year' | 'Month' | 'Month_string' | 'Day' | 'Hour' | 'Minute' |
                                       'Second' | 'Millisecond' | 'Pid' | 'File' | 'Line' | 'Module' |
                                       'Function' | 'UserStr' | 'UserStrLine' | 'DataTime' | 'Error' |
                                       'Warning' | 'Info' | 'Trace' | 'Debug'.

-type chronica_param_coloring() :: {foreground, Color :: chronica_color()} |
                                   {background, Color :: chronica_color()} |
                                   {bold, true | false}.

-record(chronica_coloring,
        {
            colored = false         :: true | false | any(),
            colors_spec = []        :: [{NameAtom :: chronica_name_atom_coloring(),
                    [ParamColoring  :: chronica_param_coloring()]}] | any(),
            formats_name = []       :: [{chronica_format_id(), chronica_format_id()}] | any(),
            end_line_formats_name = "_COLORED" :: string() | any()
        }).

-type ilfilter() :: none | error | warning | info | debug.
-type ilparam()  :: {file, string(), {integer(), integer()}, ilfilter()} | {tty, ilfilter()}.

-record(chronica_config,
        {
            rules = []                                                              :: [#chronica_rule{}] | any(),
            flows = []                                                              :: [#chronica_flow{}] | any(),
            formats = []                                                            :: [#chronica_format{}] | any(),
            colors = #chronica_coloring{}                                           :: #chronica_coloring{} | any(),
            active = false                                                          :: true | false | any(),
            detail_info = false                                                     :: true | false | any(),
            rotate_at_start = false                                                 :: true | false | any(),
            internal_logger = [{file,"chronica",{101048576,1},info}, {tty,error}]   :: [ilparam()] | any(),
            data_root =  "./cache_<Node>/"                                          :: string() | any(), % use cache <Node>
            log_root = "./log_<Node>/"                                              :: string() | any(), % use <Node> <Year> <Month> <Day> <Hour> <Minute> <Second> in string
            max_file_size = 1024*1024*10                                            :: pos_integer() | any(), % bytes
            max_file_num = 10                                                       :: pos_integer() | any(),
            tty_enabled = true                                                      :: true | false | any(),
            tcp_port = undefined                                                    :: integer() | undefined | any(),
            tcp_host = any                                                          :: any | nonempty_string() | inet:ip_address() | any(),
            backend_modules = []                                                    :: [{Name :: atom(), ModuleName :: atom()}] | any()
        }).