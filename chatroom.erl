-module(chatroom).

-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message);
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
end,
    loop(NewState).

%% This function should register a new client to this chatroom
do_register(State, Ref, ClientPID, ClientNick) ->
%%    io:format("chatroom:do_register(...)~n"),
	M = case  maps:find(ClientPID,State#chat_st.registrations) of
		{ok,_} -> State#chat_st.registrations;
		error -> maps:put(ClientPID, ClientNick ,State#chat_st.registrations)
	end,
%%	print_map(M),
	ClientPID!{self(),Ref,connect, State#chat_st.history},
	next_state(State#chat_st.name,M,State#chat_st.history).

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
%%    io:format("chatroom:do_unregister(...)~n"),
		M = maps:remove(ClientPID,State#chat_st.registrations),
%%		print_map(M),
		next_state(State#chat_st.name,M,State#chat_st.history).

%% This function should update the nickname of specified client.
do_update_nick(State, ClientPID, NewNick) ->
%%    io:format("chatroom:do_update_nick(...)~n"),
		M = case  maps:find(ClientPID,State#chat_st.registrations) of
				error -> State#chat_st.registrations;
				{ok,_} -> maps:update(ClientPID, NewNick ,State#chat_st.registrations)
			end,
%%		print_map(M),
		next_state(State#chat_st.name,M,State#chat_st.history).

get_clientname_by_clientpid(ClientPid,State)->
	case  maps:find(ClientPid,State#chat_st.registrations) of
		{ok,P} -> P;
		error-> error
	end.
%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
do_propegate_message(State, Ref, ClientPID, Message) ->
%%    io:format("chatroom:do_propegate_message(...)~n"),
		ClientPID!{self(),Ref,ack_msg},
		ClientPIDs = maps:filter(fun(K,_) -> K =/= ClientPID end,State#chat_st.registrations),
		ClientPID_New =  maps:keys(ClientPIDs),
		ClientName	= get_clientname_by_clientpid(ClientPID,State),
		lists:map(fun (Id) ->
			Id!{request, self(), Ref, {incoming_msg, ClientName, State#chat_st.name, Message}}
						end, ClientPID_New),
	  next_state(State#chat_st.name,State#chat_st.registrations,
			lists:append(State#chat_st.history,[{get_clientname_by_clientpid(ClientPID,State),Message}])).

%%print_map(Cons) ->
%%	maps:fold(
%%		fun(K, V, ok) ->
%%			io:format("Chatroom ~p: ~p~n", [K, V])
%%		end, ok, Cons).

next_state(ChatName,Reg,History)->
	#chat_st{name = ChatName,
		registrations = Reg, history = History}.