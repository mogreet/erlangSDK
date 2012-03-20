%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright 2012 Daniel YANISSE
% 
% This file is part of The Mercury Erlang SDK.
% 
% The Mercury Erlang SDK is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% The Mercury Erlang SDK is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with The Mercury Erlang SDK.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(mercury).

-behaviour(gen_server).

-include_lib("erlsom/include/erlsom.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%% API
-export([
		start_link/1,
		stop/0,
		system_ping/0,
		transaction_send/1,
		transaction_lookup/1,
		user_getopt/1,
		user_setopt/1,
		user_uncache/1,
		user_info/1,
		user_transactions/1
		]).
-export([
		keyword_list/1,
		keyword_check/1,
		keyword_add/1,
		keyword_remove/1,
		list_list/1,
		list_info/1,
		list_create/1,
		list_append/1,
		list_prune/1,
		list_send/1,
		list_download/1,
		list_empty/1,
		list_destroy/1
		]).
		
-define(SERVER, ?MODULE).

-record(state, {client_id, token}). %TODO rename state credentials

% Examples:

% mercury:start_link([{"client_id","YOUR_CLIENT_ID"},{"token","YOUR_TOKEN"}]).
% mercury:system_ping().
% mercury:transaction_send([{"campaign_id","1234"},{"to","1234567890"}, {"from","1234567890"},{"message","hello%20you"}, {"content_id","123"}]).
% mercury:transaction_lookup([{"message_id","123"},{"hash",fhds78h348fhiwf"}]).
% mercury:user_getopt([{"number", "1234567890"}]).
% mercury:user_setopt([{"campaign_id","1234"},{"number","1234567890"},{"status_code","1"}]).
% mercury:user_uncache([{"number", "1234567890"}]).
% mercury:user_info([{"number", "1234567890"}]).
% mercury:user_transactions([{"number", "1234567890"}]).

% mercury:keyword_list([]).
% mercury:keyword_check([{"keyword","TEST"}]).
% mercury:keyword_add([{"campaign_id","1234"}, {"keyword","YOURTEST"}]).
% mercury:keyword_remove([{"campaign_id","1234"}, {"keyword","YOURTEST"}]).

% mercury:list_list([]).
% mercury:list_info([{"list_id", "4321"}]).
% mercury:list_create([{"name", "LISTTEST"}]).
% mercury:list_append([{"list_id", "4321"}, {"numbers", "1234567890,1234567891"}]).
% mercury:list_prune([{"list_id","4321"}, {"numbers", "1234567890,1234567891"}]).
% mercury:list_send([{"list_id","4321"}, {"campaign_id","1234"}, {"subject","hello"}, {"message","hello%20you"}, {"content_id","123"}]).
% mercury:list_download([{"list_id","4321"}]).
% mercury:list_empty([{"list_id","4321"}, {"name","LISTTEST"}]).
% mercury:list_destroy([{"list_id","4321"}, {"name","LISTTEST"}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
	ArgCheck = check_arguments(["client_id", "token"], Args),
        case ArgCheck of
                true -> gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []);
                _ -> {error, ArgCheck} %print error message
        end.

stop() ->
	gen_server:cast(?SERVER, stop).

system_ping() ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/system.ping", []),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),

        case Status of
                "success" ->
                        [{status,Status},{code,Code},{message,Message}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "Unknown API response status"}
        end.	

transaction_send(Args) ->
	ArgCheck = check_arguments(["campaign_id", "to", "from", "message"], Args),
	case ArgCheck of
		true -> send_request(Args);
		_ -> {error, ArgCheck} %print error message
	end.

transaction_lookup(Args) ->
	ArgCheck = check_arguments(["message_id", "hash"], Args),
        case ArgCheck of
                true -> lookup_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

user_getopt(Args) ->
	ArgCheck = check_arguments(["number"], Args),
        case ArgCheck of
                true -> getopt_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

user_setopt(Args) ->
	ArgCheck = check_arguments(["campaign_id", "number", "status_code"], Args),
        case ArgCheck of
                true -> setopt_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

user_uncache(Args) ->
	ArgCheck = check_arguments(["number"], Args),
        case ArgCheck of
                true -> uncache_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

user_info(Args) ->
	ArgCheck = check_arguments(["number"], Args),
        case ArgCheck of
                true -> info_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

user_transactions(Args) ->
	ArgCheck = check_arguments(["number"], Args),
        case ArgCheck of
                true -> transactions_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

%%%===================================================================
%%% Campaign Manager API
%%%===================================================================

keyword_list(Args) ->
	ArgCheck = check_arguments([], Args),
        case ArgCheck of
                true -> k_list_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

keyword_check(Args) ->
	ArgCheck = check_arguments(["keyword"], Args),
        case ArgCheck of
                true -> k_check_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

keyword_add(Args) ->
	ArgCheck = check_arguments(["campaign_id", "keyword"], Args),
        case ArgCheck of
                true -> k_add_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

keyword_remove(Args) ->
	ArgCheck = check_arguments(["campaign_id", "keyword"], Args),
        case ArgCheck of
                true -> k_remove_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_list(Args) ->
	ArgCheck = check_arguments([], Args),
        case ArgCheck of
                true -> l_list_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_info(Args) ->
	{error, "method not functional yet: waiting for the API to be fixed (missing \"message\" field in xml reply"}.
	%ArgCheck = check_arguments(["list_id"], Args),
        %case ArgCheck of
        %        true -> l_info_request(Args);
        %        _ -> {error, ArgCheck} %print error message
        %end.

list_create(Args) ->
	ArgCheck = check_arguments(["name"], Args),
        case ArgCheck of
                true -> l_create_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_append(Args) ->
	ArgCheck = check_arguments(["list_id", "numbers"], Args),
        case ArgCheck of
                true -> l_append_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_prune(Args) ->
	ArgCheck = check_arguments(["list_id", "numbers"], Args),
        case ArgCheck of
                true -> l_prune_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_send(Args) ->
	ArgCheck = check_arguments(["list_id","campaign_id","subject","message",
			"content_id"], Args),
        case ArgCheck of
                true -> l_send_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_download(Args) ->
	ArgCheck = check_arguments(["list_id"], Args),
	FormatCheck = check_arguments(["format"],Args),
        case ArgCheck of
                true -> 
			case FormatCheck of
				true -> csv_not_implemented;
				_ -> l_download_request(Args)
			end;
                _ -> {error, ArgCheck} %print error message
        end.

list_empty(Args) ->
	ArgCheck = check_arguments(["list_id","name"], Args),
        case ArgCheck of
                true -> l_empty_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

list_destroy(Args) ->
	ArgCheck = check_arguments(["list_id","name"], Args),
        case ArgCheck of
                true -> l_destroy_request(Args);
                _ -> {error, ArgCheck} %print error message
        end.

%%%===================================================================
%%% internal functions
%%%===================================================================

check_arguments([Key | RestOfKeys], ArgList) -> 
	case lists:keyfind(Key,1,ArgList) of
		{Key, _} ->
			check_arguments(RestOfKeys, ArgList);
		false -> 
			"missing argument: " ++ Key
	end;
check_arguments([], _) ->
	true.

send_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/transaction.send", Args),
	{Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),	
        case Status of
                "success" ->
                        {"message_id",_,[MessageID]} = lists:keyfind("message_id",1, ResponseContent),
                        {"hash",_,[Hash]} = lists:keyfind("hash",1, ResponseContent),
                       	[{status,Status},{code,Code},{message,Message},{message_id,MessageID},{hash,Hash}];
                "error" -> 
                       	[{status,Status},{code,Code},{message,Message}];
                _ -> 
                        {error, "unknown API response status"}
        end.

lookup_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/transaction.lookup", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			{"campaign_id",_,[CampaignId]} = lists:keyfind("campaign_id",1, ResponseContent),
			{"to",_,[To]} = lists:keyfind("to",1, ResponseContent),
			{"from",_,[From]} = lists:keyfind("from",1, ResponseContent),
			{"to_name",_,[ToName]} = lists:keyfind("to_name",1, ResponseContent),
			{"from_name",_,[FromName]} = lists:keyfind("from_name",1, ResponseContent),
			{"content_id",_,[ContentId]} = lists:keyfind("content_id",1, ResponseContent),
			{"status",_,[TransacStatus]} = lists:keyfind("status",1, ResponseContent),
			{"history",_,HistoryContent} = lists:keyfind("history",1, ResponseContent),
			History = [ {Transac ,TimeStamp} || {"event",[TimeStamp], [Transac]} <- HistoryContent],
			[{status,Status},{code,Code},{message,Message},{campaign_id,CampaignId},
				{to,To},{from,From},{to_name,ToName},{from_name,FromName},{content_id,ContentId},
				{transaction_status,TransacStatus},{events_list,History}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

getopt_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/user.getopt", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			{"number",_,[Number]} = lists:keyfind("number",1, ResponseContent),		
			CampaignsStat = [ {CampId, CampCode, CampMsg} || 
					{"campaign", [{"id", CampId}],[{"status",[{"code", CampCode}],[CampMsg]}]} 
					<- ResponseContent],
			[{status,Status},{code,Code},{message,Message},{number,Number},{campaigns_list,CampaignsStat}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

setopt_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/user.setopt", Args),
	{Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
	case Status of
                "success" ->
                        {"number",_,[Number]} = lists:keyfind("number",1, ResponseContent),
			CampaignContent = lists:keyfind("campaign",1, ResponseContent),
			{"campaign",[{"id", CampaignId}],[{"status",[{"code", CampaignStatusCode}], [CampaignStatus]}]} 
				= CampaignContent,
			[{status,Status},{code,Code},{message,Message},{number,Number},{campaign_id,CampaignId},
				{campaign_status,CampaignStatus},{campaign_status_code,CampaignStatusCode}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

uncache_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/user.uncache", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        {"number",_,[Number]} = lists:keyfind("number",1, ResponseContent),
			[{status,Status}, {code,Code}, {message,Message}, {number,Number}];
		"error" ->
			[{status,Status},{code,Code},{message,Message}];
		_ ->
			{error, "unknown API response status"}
	end.

info_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/user.info", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			{"number",_,[Number]} = lists:keyfind("number",1, ResponseContent),
			{"carrier",[{"id",CarrierId}],[CarrierName]} = lists:keyfind("carrier",1, ResponseContent),
			{"handset",[{"id",HandsetId}],[HandsetName]} = lists:keyfind("handset",1, ResponseContent),
			[{status,Status},{code,Code},{message,Message},{number,Number},{carrier_id,CarrierId}, 
			{carrier_name,CarrierName},{handset_id,HandsetId},{handset_name,HandsetName}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

transactions_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("moms/user.transactions", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			TransactionsList = [ {CampName,CampId,MsgId,Hash,TimeStamp,To,ToName,From,FromName} ||
					{"campaign",[{"name",CampName},{"id",CampId}],
					[{"transaction",[{"message_id",MsgId},{"hash",Hash},{"datestamp",TimeStamp}],
					[{"from",[{"number",From}],[FromName]},{"to",[{"number",To}],[ToName]}]}]}
					<- ResponseContent],
		 	[{status,Status},{code,Code},{message,Message},{transactions_list,TransactionsList}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

k_list_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/keyword.list", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			[_,{"keywords",_,KeywordsContent}] = ResponseContent,
			KeywordsList = [ {IsMaster, Keyword} || {"keyword",[{"master", IsMaster}],[Keyword]} 
				<- KeywordsContent],
			[{status,Status},{code,Code},{message,Message},{keywords_list,KeywordsList}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

k_check_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/keyword.check", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			[_,{"available",_, [IsAvailable]}] = ResponseContent,
			[{status,Status},{code,Code},{message,Message},{available,IsAvailable}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

k_add_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/keyword.add", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" -> 
			[{status,Status},{code,Code},{message,Message}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.
	
k_remove_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/keyword.remove", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" -> 
                        [{status,Status},{code,Code},{message,Message}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_list_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.list", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
			[_,{"lists",_, ListsContent}] = ResponseContent,
			Lists = [ {ListId, ListName, ListType} || 
				{"list",[{"id",ListId}],[{"name",_,[ListName]},{"type",_,[ListType]}]} 
				<- ListsContent ],
			[{status,Status},{code,Code},{message,Message},{lists,Lists}];
		"error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_info_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.info", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        [_,{"list",[{"id",ListId}],[{"name",_,[ListName]},{"type",_,[ListType]},
				{"total",_,[ListTotal]},{"created",_,[ListCreated]},
				{"updated",_,[ListUpdated]},{"last_used",_,[ListLastUsed]}]}] = ResponseContent,
			[{status,Status},{code,Code},{message,Message},{list_id,ListId},{list_name,ListName},
				{list_type,ListType},{list_total,ListTotal},{list_created,ListCreated},
				{list_updated,ListUpdated},{list_last_used,ListLastUsed}];                
		"error" ->
			[{status,Status},{code,Code},{message,Message}];
               _ ->
                        {error, "unknown API response status"}
        end.

l_create_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.create", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        [_,{"list",[{"id",ListId}],_}] = ResponseContent,
			[{status,Status},{code,Code},{message,Message},{list_id,ListId}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_append_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.append", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        [_,{"statistics",_,[{"created",_,[Created]},{"duplicate",_,[Duplicate]},
				{"rejected",_,[Rejected]}]}] = ResponseContent,
			[{status,Status},{code,Code},{message,Message},{stats_created,Created},
				{stats_duplicate,Duplicate},{stats_rejected,Rejected}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_prune_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.prune", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        [_,{"statistics",_,[{"deleted",_,[Deleted]},{"not_found",_,[NotFound]},
				{"rejected",_,[Rejected]}]}] = ResponseContent,
                        [{status,Status},{code,Code},{message,Message},{stats_deleted,Deleted},
				{stats_notfound,NotFound},{stats_rejected,Rejected}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_send_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.send", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),
        case Status of
                "success" ->
                        [{status,Status},{code,Code},{message,Message}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_download_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.download", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),

        case Status of
                "success" ->
                        [_,{"recipients",_,RecipientsContent}] = ResponseContent,
			RecipientsList = [{Number,TimeZone} || {"recipient",_,[{"number",_,[Number]},
				{"time_zone",_,[TimeZone]}]} <- RecipientsContent],
			[{status,Status},{code,Code},{message,Message},{recipients_list,RecipientsList}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_empty_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.empty", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),

        case Status of
                "success" ->
                        [{status,Status},{code,Code},{message,Message}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

l_destroy_request(Args) ->
	{ResponseArgs, ResponseContent} = send_http_request("cm/list.destroy", Args),
        {Status, Code, Message} = read_standard_response(ResponseArgs, ResponseContent),

        case Status of
                "success" ->
                        [{status,Status},{code,Code},{message,Message}];
                "error" ->
                        [{status,Status},{code,Code},{message,Message}];
                _ ->
                        {error, "unknown API response status"}
        end.

getState() ->
gen_server:call(?SERVER, getState).

build_url(Method, Cred, Args) ->
	APIUrl = "https://api.mogreet.com/",
	BaseUrl = APIUrl ++ Method ++ "?" ++ "client_id=" ++ Cred#state.client_id ++ "&token=" ++ Cred#state.token,
	build_url(Args, BaseUrl).
build_url([Arg | RestOfArgs], Url) ->
	NewUrl = Url ++ "&" ++ element(1, Arg) ++ "=" ++ element(2, Arg),
	build_url(RestOfArgs, NewUrl);
build_url([], Url) ->
	Url.

send_http_request( Method, Args) ->
        Credentials = getState(),
        RequestUrl = build_url( Method, Credentials, Args),
        {ok, {{_, 200, _}, _, Body}} = httpc:request(RequestUrl),
        ParsedBody = erlsom:simple_form(Body),
        {ok, {_,ResponseArgs, ResponseContent}, _ } = ParsedBody,
        {ResponseArgs, ResponseContent}.

read_standard_response(ResponseArgs, ResponseContent) ->
	{"status", Status} = lists:keyfind("status",1,ResponseArgs),
        {"code", Code} = lists:keyfind("code",1,ResponseArgs),
        {"message",_,[Message]} = lists:keyfind("message",1, ResponseContent),
	{Status, Code, Message}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

      % init() sends the ping request, parses the XML response and stores data to the state.
init(Args) ->
	inets:start(),
	ssl:start(),
	{"client_id", ClientId} = lists:keyfind("client_id",1,Args),
	{"token", Token} = lists:keyfind("token",1,Args),
	{ok, #state{client_id = ClientId, token = Token}, 0}.

handle_call(getState, _From,  State) ->
{reply, State, State}.

handle_cast(stop, State) ->
{stop, normal, State}.

handle_info(_Info, State) ->
{noreply, State}.

terminate(_Reason, _State) ->
ok.

code_change(_OldVsn, State, _Extra) ->
{olngIndent, State}.
