[![Build Status](https://travis-ci.org/eltex-ecss/chronica.svg?branch=master)](https://travis-ci.org/eltex-ecss/chronica)
<p align="center">
<img src="https://raw.githubusercontent.com/eltex-ecss/chronica/master/doc/chronica.jpg"/>
</p>
[This document translated on russian](https://github.com/eltex-ecss/chronica/blob/master/README_RU.md)

## Overview
Chronica is a framework for logging messages for Erlang OTP Applications.

## Features
* [Faster, lightweight, more flexible!](https://docs.google.com/document/d/1S4-Yf799d5SDCWhr78Fsm6-EY98gd1BRW-Qffaynzsc/edit?usp=sharing)
* Pinning log flows to different outputs (file, tty, network)
* Custom rules (as regexp) and text formatting for output are supported.
* Possibility to define text formatting separately for each backend
* Tools for remote connection allows you to set rule and get back log flow
    filtered by that rule (grablog)
* Easy migration from lager
* Logging functions which are unused because of current verbose level causes
    no overhead (deleting unused calls for log level on-the-fly)
* Write on disk in two modes: text and binary
* Custom backends for output logs
* Supports unicode
* Log rotate
* Colored output to terminal
* Change log level and rules in runtime
* Custom tags for easy filtered logs

## Usages
In order to add Chronica into your application you will need to add it as Erlang
dependency in app file and also set compiler options:
```erlang
{parse_transform, pt_chronica}
```
Or add include file in each module if it uses Chronica:
```erlang
-include_lib("chronica/include/chronica.hrl").
```

## Supported log levels:
```
(MAX) debug -> trace -> info -> warning -> (MIN) error
```

And custom tags defines (for easy split on different output flows) as atom in
compile time. In all logging messages add service information (module name,
line number in source code when located this logging message).

Formats for call log:
```erlang
log:Level("String"),                 %% log:info("Hello world!")
log:Level("Format", [Args]),         %% log:error("Crush from ~p", [self()])
log:Level(Tag, "Format", [Args]),    %% log:debug(verbose, "Verbose tag", [])
log:Level([Tags], "Format", [Args]), %% log:trace([a, b], "Multiple tag", [])
Tag := atom
```

Or usage macros: ?DBG, ?TRACE, ?INFO, ?WARN, ?ERR from header file:
```erlang
-include_lib("chronica/include/chronica_macro.hrl").
```

Because of function log:Level replace on inner Chronica functions in compile
time next example will surprise you:
```erlang
log:info("My stacktrace: ~p", [erlang:get_stacktrace()])
```
and replace on
```erlang
log:info("My stacktrace: %stack", [])
```

## log:todo:
Applies developers instead unused variable:
```erlang
    garbage() ->
        ...
        TODO_DELETE_THIS_METHOD,
        ...
erlc ...
variable 'TODO_DELETE_THIS_METHOD' is unused
```

Example:
```erlang
    garbage() ->
        ...
        log:todo("Delete this method")
        ...
erlc ...
TODO: Delete this method
```

## Configs:
Add default section to sys.config:
```
{chronica, [
    Rules,
    Flows,
    Formats,
    InternalLogger,
    InternalLoggerFilename,
    LogRoot,
    MaxFileSize,
    MaxFileNum,
    LogIfacePath
   ]}
```

[Example](https://github.com/eltex-ecss/chronica/blob/master/samples/sys.config)

### Options:
#### Rules:
List rules for filtered log in logging flow. Rule contains name, regexp for
filtered message (may be include module, user tags), logging level (error,
warning, info, trace, debug) list flow and toggle. Regexp may be contain special
character:
* "|" or
* "&" and
* "*" null or more any character
* "?" one ant character
* "!" not

```erlang
{warning_example1, “*”, warning, [warning_file], off},
```
this rule include all warning message in warning_file flow but turn off,

```erlang
{warning_example2, “mysql*&!mysql_ag*”, error, [tty, error], on},
```
this rule include error message from modules beginning mysql besides mysql_ag
and output in tty and error flows

#### Flows:
Named streams for output process and provided backens (tty, file, udp).
Include mode for saving data (binary - lightweight for big intensity and text)
and name formats.

For example:

```erlang
{warning_file, [{file, "warning.log"}]},
```
Define "warning_file" flow output data in file with "warning.log" filename in
default format and text mode.

```erlang
{warning_file, [{file, "warning.log", binary}]},
```
As previous example but in binary mode

```erlang
{warning_file, [{tty, custom_format}]},
```
Flow output in terminal in "custom_format" format

```erlang
{journald, [{journald, short}]},
```
Flow output in journald (if exist)

#### Formats:
Define output format for message. Comprises from service macros beginig in '%'.
Default configuration already have one formats: 'default', but you may be
override him.

Examples:
```erlang
 {full, "%Id %Y-%M(%Ml)-%D %H:%Mi:%S:%Ms %PRIORITY %P %Priority %Pid %File %Line %Module %Function %Message %MessageLine\n"}
 {default, "%Y-%M-%D %H:%Mi:%S:%Ms %PRIORITY %Pid [%Module:%Line]: %Message\n"},
 {short, "%Y-%M-%D %H:%Mi:%S:%Ms %P %Pid [%Module:%Line]: %Message\n"}
```

```erlang
default flow
{flows, [{screen, [{tty, default}]}] }
Out:
    2015-09-27 14:59:32.012401 WARN  <0.130.0> [testing_chronica_logs:testing_short_warning_file/1:57]: test chronica
```

Override default:
```
{flows, [ {screen, [ {tty, default} ] } ] },
{formats,[{default, "%H:%Mi:%S.%Ms [%Priority] %Message\n"}]},
Out:
    15:03:15.702088 [warning] test chronica
```

If you want define custom formatter:
```erlang
{flows, [ {file, "warning_log", my_default} ] },
{formats, [ {my_default, “%Message”} ] }
```
## Compilation modes
Chronica supports three compilation modes
* <b>chronica optimization</b>. Optimization mode (Default). In this mode, Chronica generates warnings about the initialized variables in the function body, that are not used anywhere except log:level(...). To avoid receiving warnings, declare variables as \_Var or Var\_.
* <b>chronica default</b>. Mode with optimization turned off. Early this mode was the default.
* <b>chronica disabled</b>. The mode in which chronica cuts out log:level(...).

#### Compilation Options
Are used to define the behavior at compile time chronica. The options can be set in two ways
* Using environment variables:
    * CHRONICA_MATCH_IGNORED_VAR
    * CHRONICA_DISABLED
    * CHRONICA_DEFAULT
* In the rebar.config section of erl_opts
    * chronica_match_ignored_var
    * chronica_disabled
    * chronica_default

#### CHRONICA_MATCH_IGNORED_VAR || chronica_match_ignored_var
The options is used in the <b>chronica optimization</b> mode and allows you to output variables that have been declared as \_Var or Var\_

#### CHRONICA_DISABLED || chronica_disabled
The options is used to activate the mode <b>chronica disabled</b>

#### CHRONICA_DEFAULT || chronica_default
The options is used to activate the mode <b>chronica default</b>

## Runtime API
### For update Chronica configuration in runtime usages:

* chronica_manager:active(false | true)

Toggle chronica

* chronica_manager:update_rule_inwork(name_rule, false | true)

Toggle "name_rule" rule on the fly.

* chronica_manager:add_application(App)

Add new application in list logging application

* chronica_manager:add_rule(Rule, Regexp, Level, Flow)
* chronica_manager:add_rule(Rule, Regexp, Level, Flow, Fun)

Add new rule in list logging rule and activates

<p align="center">
<img src="https://github.com/eltex-ecss/chronica/blob/master/doc/logo_eltex.jpg"/>
</p>
