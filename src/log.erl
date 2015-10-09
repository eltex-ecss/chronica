%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%   NOTE: DON'T CHANGE THIS MODULE. Used only for parse-transform.
%%% @end
%%%-------------------------------------------------------------------
-module(log).

-export([
         debug/1,
         debug/2,
         debug/3,
         info/1,
         info/2,
         info/3,
         todo/1,
         todo/2,
         todo/3,
         trace/1,
         trace/2,
         trace/3,
         warning/1,
         warning/2,
         warning/3,
         error/1,
         error/2,
         error/3,
         log/6
        ]).

debug(_) -> ok.
debug(_, _) -> ok.
debug(_, _, _) -> ok.

info(_) -> ok.
info(_, _) -> ok.
info(_, _, _) -> ok.

todo(_) -> ok.
todo(_, _) -> ok.
todo(_, _, _) -> ok.

trace(_) -> ok.
trace(_, _) -> ok.
trace(_, _, _) -> ok.

warning(_) -> ok.
warning(_, _) -> ok.
warning(_, _, _) -> ok.

error(_) -> ok.
error(_, _) -> ok.
error(_, _, _) -> ok.

log(_, _, _, _, _, _) -> ok.