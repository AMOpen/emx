%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @copyright 2008 Vitor Rodrigues
%% @author Vitor Rodrigues <vitor@tarpipe.com>
%% @contributor Yoan Blanc <yoan@dosimple.ch>
%% @version {@version}
%%
%% @doc
%% <h1>Elang API to CouchDb</h1>
%% eCouch is an application that provides an API to a CouchDb server
%% It uses the <a href="http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang">rfc4627</a> module from <a href="http://www.lshift.net/">LShift</a>
%% The design was inspired in the article <a href="http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles">Building a Non-blocking TCP server using OTP principles</a>
%% and assumes that <a href="http://www.erlang.org/doc/apps/inets/index.html">inets</a> application is running.
%% todo:
%% Accept a list of servers and implement load distribution algorithms <br/>
%% Implement views
%%
%% @end
%% =====================================================================

-module(ecouch).
-author('Vitor Rodrigues').
-author('Yoan Blanc').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/1, start_client/0]).

%% API
-export([
        db_create/1,
        db_delete/1,
        db_list/0,
        db_info/1,
        doc_create/2,
        doc_create/3,
        doc_bulk_create/2,
        doc_update/3,
        doc_bulk_update/2,
        doc_delete/3,
        doc_bulk_delete/2,
        doc_get/2,
        doc_get/3,
        doc_get_all/1,
        doc_get_all/2,
        view_create/3,
        view_update/3,
        view_delete/3,
        view_get/2,
        view_get/3,
        view_adhoc/2,
        view_adhoc/3,
        view_access/3,
        view_access/4,
	url_get/2,
	url_post/2,
	url_put/2,
	url_delete/2
        ]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
%% Define the timeout for gen_server calls to be something longer than 5 seconds.
-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(_Type, StartArgs::startargs()) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%%
%% @type startargs() = {host(), tcp_port()}
%% @type host() = string()
%% @type tcp_port() = int()
%%
%% @doc This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------

start(_Type, {Host, Port, User, Pass}) ->
    case get_app_opt(host, Host) of
        none ->
            {error, "Missing required config option 'host'"};
        HostVal ->
            case get_app_opt(port, Port) of
                none ->
                    {error, "Missing required config option 'port'"};
                PortVal ->
                    case get_app_opt(user, User) of
                        none ->
                            supervisor:start_link({local, ?MODULE},
                                                  ?MODULE,
                                                  [HostVal, PortVal,
                                                   none, none]);
                        UserVal ->
                            case get_app_opt(pass, Pass) of
                                none ->
                                    {error, "Missing required config options 'pass'"};
                                PassVal ->
                                    supervisor:start_link({local, ?MODULE},
                                                          ?MODULE,
                                                          [HostVal, PortVal,
                                                           UserVal, PassVal])
                            end
                    end
            end
    end.


%% @hidden

start_client() ->
    supervisor:start_child(ec_client_sup, []).

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%% @hidden

init([Host, Port, User, Pass]) ->
    % making the init variable accessible through env (used by the tests)
    case application:get_application() of
        {ok, Application} ->
            application:set_env(Application, host, Host),
            application:set_env(Application, port, Port),
            application:set_env(Application, user, User),
            application:set_env(Application, pass, Pass);
        _ -> ok
    end,
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % eCouch Listener
              {   ec_listener_sup,                         % Id       = internal id
                  {ec_listener,start_link,[Host, Port, User, Pass]},   % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [ec_listener]                            % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   ec_client_sup,
                  {supervisor,start_link,[{local, ec_client_sup}, ?MODULE, [ec_client]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

%% @hidden

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % HTTP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% API Functions

%% @spec db_create(DatabaseName::string()) -> ok | {error, Reason::term()}
%%
%% @doc Create a database

db_create(DatabaseName) ->
    Path = lists:flatten(io_lib:format("/~s/", [DatabaseName])),
    Reply = gen_server:call(ec_listener, {put, Path, []}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec db_delete(DatabaseName::string()) -> ok | {error, Reason::term()}
%%
%% @doc Delete a database

db_delete(DatabaseName) ->
    Path = lists:flatten(io_lib:format("/~s/", [DatabaseName])),
    Reply = gen_server:call(ec_listener, {delete, Path, []}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec db_list() -> {ok, [str()]} | {error, Reason::term()}
%%
%% @doc List databases

db_list() ->
    Path = "/_all_dbs",
    Reply = gen_server:call(ec_listener, {get, Path, []}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec db_info(DatabaseName::string()) -> {ok, Info::json()} | {error, Reason::term()}
%%
%% @type json() = obj() | array() | num() | str() | true | false | null
%% @type obj() = {obj, [{key(), val()}]}
%% @type array() = [val()]
%% @type key() = str() | atom()
%% @type val() = obj() | array() | num() | str() | true | false | null
%% @type num() = int() | float()
%% @type str() = bin()
%%
%% @doc Database info

db_info(DatabaseName) ->
    Path = lists:flatten(io_lib:format("/~s", [DatabaseName])),
    Reply = gen_server:call(ec_listener, {get, Path, []}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_create(DatabaseName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Create document

doc_create(DatabaseName, Doc) ->
    DocJson = rfc4627:encode(Doc),
    Path = lists:flatten(io_lib:format("/~s/", [DatabaseName])),
    Reply = gen_server:call(ec_listener, {post, Path, DocJson}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @hidden

doc_create(_DatabaseName, DocName, _Options) when DocName == "" ->
    {ok, {obj, [{"error", <<"not_created">>},
                {"reason", <<"invalid_name">>}]}};

%% @spec doc_create(DatabaseName::string(), DocName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Create a named document

doc_create(DatabaseName, DocName, Doc) ->
    JsonDoc = rfc4627:encode(Doc),
    Path = lists:flatten(io_lib:format("/~s/~s", [DatabaseName, DocName])),
    Reply = gen_server:call(ec_listener, {put, Path, JsonDoc}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_bulk_create(DatabaseName::string(), DocList) -> {ok, Response::json()} | {error, Reason::term()}
%%     DocList = [json()]
%%
%% @doc Batch create a set of documents.

doc_bulk_create(DatabaseName, DocList) ->
    BulkDoc = rfc4627:encode({obj, [{"docs", DocList}]}),
    Path = lists:flatten(io_lib:format("/~s/~s", [DatabaseName, "_bulk_docs"])),
    Reply = gen_server:call(ec_listener, {post, Path, BulkDoc}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_update(DatabaseName::string(), DocName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Update document

doc_update(DatabaseName, DocName, Doc) ->
    doc_create(DatabaseName, DocName, Doc).

%% @spec doc_bulk_update(DatabaseName::string(), DocListRev) -> {ok, Response::json()} | {error, Reason::term()}
%%     DocListRev = [json()]
%%
%% @doc Batch update a set of documents

doc_bulk_update(DatabaseName, DocListRev) ->
    doc_bulk_create(DatabaseName, DocListRev).

%% @hidden

doc_delete(_DatabaseName, DocName, _Rev) when DocName == "" ->
    {ok, {obj, [{"error", <<"not_deleted">>},
                {"reason", <<"invalid_name">>}]}};

%% @spec doc_delete(DatabaseName::string(), DocName::string(), Rev::string()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Delete document

doc_delete(DatabaseName, DocName, Rev) ->
    Path = lists:flatten(io_lib:format("/~s/~s", [DatabaseName, DocName])),
    Reply = gen_server:call(ec_listener, {delete, Path, [{"rev", Rev}]}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_bulk_delete(DatabaseName::string(), DocList) -> {ok, Response::json()} | {error, Reason::term()}
%%     DocList = [json()]
%%
%% @doc Batch delete a set of documents.

doc_bulk_delete(DatabaseName, DocList) ->
    BulkDoc = rfc4627:encode({obj, [{"docs", [{obj, [{"_deleted", true} | D]} || {obj,D} <- DocList]}]}),
    Path = lists:flatten(io_lib:format("/~s/~s", [DatabaseName, "_bulk_docs"])),
    Reply = gen_server:call(ec_listener, {post, Path, BulkDoc}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_get(DatabaseName::string(), DocName::string) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get document

doc_get(DatabaseName, DocName) ->
    doc_get(DatabaseName, DocName, []).

%% @hidden

doc_get(_DatabaseName, DocName, _Options) when DocName == "" ->
    {ok, {obj, [{"error", <<"not_found">>},
                {"reason", <<"invalid_name">>}]}};

%% @spec doc_get(DatabaseName::string(), DocName::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get document

doc_get(DatabaseName, DocName, Options) ->
    Path = lists:flatten(io_lib:format("/~s/~s", [DatabaseName, DocName])),
    Reply = gen_server:call(ec_listener, {get, Path, Options}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec doc_get_all(DatabaseName::string()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get all documents

doc_get_all(DatabaseName) ->
    doc_get_all(DatabaseName, []).

%% @spec doc_get_all(DatabaseName::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get all documents

doc_get_all(DatabaseName, Options) ->
    Path = lists:flatten(io_lib:format("/~s/_all_docs", [DatabaseName])),
    Reply =
        case proplists:lookup(keys, Options) of
            none ->
                gen_server:call(ec_listener, {get, Path, Options}, ?DEFAULT_TIMEOUT);
            {keys, _Keys} = Data ->
                JsonKeys = rfc4627:encode({obj, [Data]}),
                NewOptions = proplists:delete(keys, Options),
                gen_server:call(ec_listener, {post, Path, JsonKeys, NewOptions}, ?DEFAULT_TIMEOUT)
        end,
    handle_reply(Reply).

%% @hidden

view_create(DatabaseName, DesignName, Views) ->
    JsonDoc = rfc4627:encode({obj, [{language, <<"javascript">>},
                                    {views, Views}]}),
    Path = lists:flatten(io_lib:format("/~s/_design/~s", [DatabaseName, DesignName])),
    Reply = gen_server:call(ec_listener, {put, Path, JsonDoc}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @hidden

view_update(DatabaseName, DesignName, Funs) ->
    view_create(DatabaseName, DesignName, Funs).

%% @hidden

view_delete(DatabaseName, DesignName, Rev) ->
    Path = lists:flatten(io_lib:format("/~s/_design/~s", [DatabaseName, DesignName])),
    Reply = gen_server:call(ec_listener, {delete, Path, [{"rev", Rev}]}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @hidden

view_get(DatabaseName, DesignName, Options) ->
    Path = lists:flatten(io_lib:format("/~s/_design/~s", [DatabaseName, DesignName])),
    Reply = gen_server:call(ec_listener, {get, Path, Options}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @hidden

view_get(DatabaseName, DesignName) ->
    view_get(DatabaseName, DesignName, []).

%% @spec view_adhoc(DatabaseName::string(), Fun::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%%
%% @doc Access an adhoc view

view_adhoc(DatabaseName, Fun) ->
    view_adhoc(DatabaseName, Fun, []).

%% @spec view_adhoc(DatabaseName::string(), Fun::json(), Options::options()) -> {ok, Response::json} | {error, Reason::term()}
%%
%% @doc Access an adhoc view

view_adhoc(DatabaseName, Fun, Options) ->
    Path = lists:flatten(io_lib:format("/~s/_temp_view", [DatabaseName])),
    Reply = gen_server:call(ec_listener, {post, Path, Fun, "text/javascript", Options}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @hidden

view_access(DatabaseName, DesignName, ViewName) ->
    view_access(DatabaseName, DesignName, ViewName, []).

%% @hidden

view_access(DatabaseName, DesignName, ViewName, Options) ->
    Path = lists:flatten(io_lib:format("/~s/_design/~s/_view/~s", [DatabaseName, DesignName, ViewName])),
    Reply =
        case proplists:lookup(keys, Options) of
            none ->
                gen_server:call(ec_listener, {get, Path, Options}, ?DEFAULT_TIMEOUT);
            {keys, _Keys} = Data ->
                JsonKeys = rfc4627:encode({obj, [Data]}),
                NewOptions = proplists:delete(keys, Options),
                gen_server:call(ec_listener, {post, Path, JsonKeys, NewOptions}, ?DEFAULT_TIMEOUT)
        end,
    handle_reply(Reply).

%% @spec url_get(Path::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get URL for generic url calls, used for specialized http handlers like _fti

url_get(Path, Options) ->
    Reply = gen_server:call(ec_listener, {get, Path, Options}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec url_post(Path::string(), Data::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc POST URL for generic url POST calls, used for specialized http handlers like _fti

url_post(Path, Data) ->
    Reply = gen_server:call(ec_listener, {post, Path, Data}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec url_put(Path::string(), Data::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Pur URL for generic url PUT calls, used for specialized http handlers like _fti

url_put(Path, Data) ->
    Reply = gen_server:call(ec_listener, {put, Path, Data}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% @spec url_get(Path::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc DELETE URL for generic url DELETE calls, used for specialized http handlers

url_delete(Path, Options) ->
    Reply = gen_server:call(ec_listener, {delete, Path, Options}, ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%%====================================================================
%% Internal functions
%%====================================================================

handle_reply(Reply) ->
    case Reply of
        {error, Reason} ->
            {error, Reason};
        R ->
              case rfc4627:decode(R) of
                  {ok, Json, _Raw} ->
                      {ok, Json};
                  {error, Reason} ->
                      {error, Reason}
              end
    end.

get_app_opt(Opt, Default) ->
    Value = case application:get_application() of
        {ok, Application} -> application:get_env(Application, Opt);
        _ -> undefined
        end,
    case Value of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error -> Default
            end
        end.
