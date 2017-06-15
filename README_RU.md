<p align="center">
<img src="https://raw.githubusercontent.com/eltex-ecss/chronica/master/doc/chronica.jpg"/>
</p>

## Общее представление
Chronica это специальный фреймворк используемый для логирования сообщений в Erlang.
Он осуществляет более простой способ введения журналов в Erlang-приложениях.

## Особенности
* [Быстрее, легче, гибче!](https://docs.google.com/document/d/1S4-Yf799d5SDCWhr78Fsm6-EY98gd1BRW-Qffaynzsc/edit?usp=sharing)
* Вывод потока логов различными способами (в файл, на экран,
    по сети)
* Настраиваемые правила вывода (в виде регулярных выражений) и форматы вывода
* Индивидуальные форматы вывода для каждого backend'a
* Утилита для удалённого получения логов (grablog) по заданным правилам
* Легкая миграция проектов, ранее использующих lager
* Функции логирования не соответствующие заданному уровню логов не нагружают
    систему (неиспользуемые вызов фунций в коде удаляется на лету)
* Запись логов в двух режимах: бинарном (быстрый) и текстовом (удобный)
* Возможность реализации и добавления собственных backend'ов
* Поддержка unicode
* Ротация логов
* Поддержка цветного терминального вывода
* Возможность изменения правил фильтрации логов и уровня логирования на лету
* Возможность создания своих тегов для простой фильтрации логов
* Быстрый старт приложения при повторном запуске на узле

##Использование
Для того, чтобы добавить Chronica в ваше приложение достаточно добавить её как
Erlang зависимость (в app файле), а также выставить опции компиляции:
```erlang
{parse_transform, pt_chronica}
```
Либо добавить заголовочный файл в каждом модуле использующем Chronica:
```erlang
-include_lib("chronica/include/chronica.hrl").
```

## Поддерживаемые уровни логирования:
```erlang
(MAX) debug -> trace -> info -> warning -> (MIN) error
```
Также доступны пользовательские теги являющиеся атомами, использующиеся для
выделения логов по каким-то критериям и расширяющие стандартные уровни
логирования.

Форматы вызова фунций логирования:
```erlang
Tag :: atom().
Tags :: [Tag()].
Level :: debug | trace | info | warning | error.

log:Level("String"),
log:Level("Format", [Args]),
log:Level(Tag, "Format", [Args]),
log:Level(Tags, "Format", [Args]),
```

Или используйте соответствующие макросы DBG, TRACE, INFO, WARN, ERR:
```erlang
LEVEL(Format)
LEVEL(Format, Args)
```

Которые описаны в заголовочном файле:
```erlang
-include_lib("chronica/include/chronica_macro.hrl").
```

## Теги
Тегирование в Chronica позволяет создать группы уникальных логов,
которые будут писаться в определенный backend для того чтобы была возможность
быстро найти нужные сообщения.
Cуществуют:

* Пользовательские теги:
    * Явные теги
    ```erlang
    log:error([my_teg], “test”).
    ```
    * Неявные теги. Указываются в начале и применяются для всех логов данного модуля
    ```erlang
    -module(name).
    -chronica_tag(Tags).
    ```

* Служебныe теги. Создаются во время compile-time. Такими тегами являются
    * Тег имени модуля
    * Тег имени модуля с указанием строки расположения данного log сообщения

Пример:
```erlang
1    -module(testing).
2    -export([test/0]).
3    -include_lib("chronica/include/chronica.hrl").
4    test() ->
5        log:error(“test”).

После компиляции у log:error появится теги данного вида [testing, testing_5]
```

Из-за того, что в Chronica вычисление аргументов при вызове фунции логирования
происходит внутри логера, вызов:
```erlang
log:Level("~p", [erlang:get_stacktrace()])

для корректного вывода стека вызовов в логи преобразуется в

begin
    Chronica_stacktrace_line = erlang:get_stacktrace(),
    log:Level("~p", [Chronica_stacktrace_line]).
end
```
В правилах хорошего тона, лучше все вызовы, контекстно зависимые от процессора, выполнять в данном процессоре, а результаты помещать в переменную.

## log:todo
Применяется для того чтобы отследить участок кода, который считается
незавершённым. Работает только с константными данными, и выводит данное
напоминие как предупреждение компилятора.

Пример:
```erlang
1    -module(testing).
2    -export([test/0]).
3    -include_lib("chronica/include/chronica.hrl").
4    test() ->
5        log:todo(“Need more tests!!!”).

Out:
    (полный путь)/chronica_testing.erl:214: TODO: Need more tests!!!
```

## Конфигурационный файл
Добавте секицю в sys.congig:
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

[Пример](https://github.com/eltex-ecss/chronica/blob/master/samples/sys.config)

### Опции:
#### Rules:
Rules - это список правил выборки сообщений в указанные Flows. Правило включает
в себя имя, регулярное сообщение для отбора сообщений (может включать имя модуля
или пользовательский тег), уровень логирования (error, warning, info, trace,
debug), список Flow (потоков)  в которыe будет происходить вывод попавших в
данное правило сообщений, а также флаг состояние данного правила (включено оно
или нет). Регулярное выражение может включать в себя служебные символы:
* "|" or
* "&" and
* "*" null or more any character
* "?" one ant character
* "!" not

Пример:
```erlang
{warning_file_example1,      “*”,    warning,    [warning_file], off},
```
данное правило будет проигнорировано

```erlang
{warning_example2, “mysql*&!mysql_ag*”, error, [tty, error], on},
```
данное правило выполнит все логи уровня error модулей начинающихся с префикса
mysql, но не mysql_ag.

```erlang
{warning_file_example2,      “my_tag”,    warning,    [warning_file], on},
данное правило выполнит все логи уровня warning или ниже, если у них
присутствует тег “my_tag”
```

#### Flows
Flows - это именованные потоки, которые пишут данные с помощью заданного backend
(в файл, по сети, на экран). Так же здесь есть возможность задать режим:
текстовый (удобный) или бинарный (быстрый).

Пример:
```erlang
{warning_file, [{file, "debug.bin", binary}]},
Поток будет писать в debug.bin, дефолтным форматом, в бинарном виде

{warning_file, [{file, "warning.txt"}]},
Поток будет писать в warning.txt, дефолтным форматом, в текстовом виде

{warning_file, [{tty, short}]},
Поток будет писать на консоль, short форматом, в текстовом виде

{journald, [{journald, short}]},
Поток будет писать в journald (Если установлен)
```

#### Formats
Задаёт форматирование текста. Состоит из макросов начинающихся со знака '%'.
По умолчанию всегда задан 'default' формат, но его можно переназначить.

```erlang
{formats,{ [ {default, “строка вывода”} ] }
```
Это переопределит стандартный способ вывода информации, для всех flows указанных
без переопределенного заголовка из поля format.
```erlang
default по умолчанию
{flows, [{screen, [{tty, default}]}] }
Out:
    2015-09-27 14:59:32.012401 WARN  <0.130.0> [testing_chronica_logs:testing_short_warning_file/1:57]: test chronica

```erlang
 {full, "%Id %Y-%M(%Ml)-%D %H:%Mi:%S:%Ms %PRIORITY %P %Priority %Pid %File %Line %Module %Function %Message %MessageLine\n"}
 {default, "%Y-%M-%D %H:%Mi:%S:%Ms %PRIORITY %Pid [%Module:%Line]: %Message\n"},
 {short, "%Y-%M-%D %H:%Mi:%S:%Ms %P %Pid [%Module:%Line]: %Message\n"}
```

## Режимы компиляции
Chronica поддерживает три режима компиляции
* <b>chronica optimization</b>. Режим оптимизации (установленный по умолчанию). В данном режиме Chronica формирует предупреждения о инициализированных переменных в теле функции, которые не используется нигде, кроме log:level(...). Для того, чтобы не получать предупреждения, объявляйте переменные как \_Var или Var\_.
* <b>chronica default</b>. Режим с выключенной оптимизацией. Раннее данный режим был по умолчанию
* <b>chronica disabled</b>. Режим в котором chronica вырезает log:level(...).

#### Опции компиляции
Предназначены для того, чтобы задать поведения chronica во время компиляции. Задаваться опции могут двумя способами
* С помощью переменных окружения:
    * CHRONICA_MATCH_IGNORED_VAR
    * CHRONICA_DISABLED
    * CHRONICA_DEFAULT
* В rebar.config в секции erl_opts
    * chronica_match_ignored_var
    * chronica_disabled
    * chronica_default

#### CHRONICA_MATCH_IGNORED_VAR || chronica_match_ignored_var
Данная опция используется только в режиме <b>chronica optimization</b> и позволяет выводить переменные, которые были объявленны как \_Var или Var\_

#### CHRONICA_DISABLED || chronica_disabled
Данная опция используется для включения режима <b>chronica disabled</b>

#### CHRONICA_DEFAULT || chronica_default
Данная опция используется для включения режима <b>chronica default</b>

## Настройка на лету
Применяются для модифицирования конфигурационного файла
* chronica_manager:update_rule_inwork(name_rule, false | true)
Включает\отключает уже существующие правила.

* chronica_manager:active(false | true)
Включает\отключает Chronica.

* chronica_manager:add_application(App)
Регистрирует application во время работы Chronica.

* chronica_manager:add_rule(Rule, Regexp, Level, Flow)
* chronica_manager:add_rule(Rule, Regexp, Level, Flow, Fun)
Добавляет новое правило во время работы Chronica и активирует его.

<p align="center">
<img src="https://github.com/eltex-ecss/chronica/blob/master/doc/logo_eltex.jpg"/>
</p>
