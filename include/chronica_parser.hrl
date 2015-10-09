%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%% Created : 22. Апр. 2015 11:31
%%% @end
%%%-------------------------------------------------------------------
-record(token, {type :: control | string,
                value :: string()}).