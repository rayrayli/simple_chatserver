-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

get_chatnames_by_clientpid(ClientPID,State)->
	ChatNames = maps:filter(fun(_,V) -> lists:member(ClientPID,V) end,State#serv_st.registrations),
	maps:keys(ChatNames).

get_chatpid_by_chatname(ChatName,State)->
	case  maps:find(ChatName,State#serv_st.chatrooms) of
		{ok,P} -> P;
		error-> error
	end.
%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
%%	io:format("server:do_join(...)~n"),
	ChatPID = case maps:find(ChatName,State#serv_st.chatrooms) of
						 {ok,R} -> R;
						 error ->  spawn(chatroom, start_chatroom ,[ChatName])
					 end,
	ClientNick = case maps:find(ClientPID,State#serv_st.nicks) of
			 {ok,N} -> N;
			 error -> error
		end,
	ChatPID !{self(),Ref,register, ClientPID, ClientNick},
	M = case  maps:find(ChatName,State#serv_st.registrations) of
		{ok,P} -> maps:update(ChatName,lists:append([ClientPID],P),State#serv_st.registrations) ;
		error -> maps:put(ChatName,[ClientPID],State#serv_st.registrations)
	end,
	Q = maps:put(ChatName,ChatPID,State#serv_st.chatrooms),
%%	io:format("server dojoin: PID ~w,ClientNick ~s ~n", [ChatPID,ClientNick]),
%%	print_map(State#serv_st.nicks),
%%	print_map(M),
%%	print_map(Q),
	next_state(State#serv_st.nicks,M,Q).

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
%%    io:format("server:do_leave(...)~n"),
		ChatPID = get_chatpid_by_chatname(ChatName,State),
		M = case  maps:find(ChatName,State#serv_st.registrations) of
		{ok,P} -> maps:update(ChatName,lists:delete(ClientPID,P),State#serv_st.registrations);
		error-> State#serv_st.registrations
			end,
		ChatPID!{self(), Ref, unregister, ClientPID},
%%		print_map(M),
		ClientPID!{self(),Ref,ack_leave},
		next_state(State#serv_st.nicks,M,State#serv_st.chatrooms).


%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
%%    io:format("server:do_new_nick(...)~n"),
		ClientPIDs = maps:filter(fun(_,V) -> V=:= NewNick end,State#serv_st.nicks),
		case maps:keys(ClientPIDs) of
			[]  ->
				M = maps:update(ClientPID,NewNick,State#serv_st.nicks),
				ChatNames = get_chatnames_by_clientpid(ClientPID,State),
				lists:map(fun (Id) ->
					   ChatPid = get_chatpid_by_chatname(Id,State),
					   ChatPid!{self(), Ref, update_nick, ClientPID,NewNick}
									end, ChatNames),
				ClientPID!{self(),Ref,ok_nick},
%%				print_map(M),
				next_state(M,State#serv_st.registrations,State#serv_st.chatrooms);
			[ _ | _ ]  ->
				ClientPID!{self(),Ref,err_nick_used},
				State
		end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
%%    io:format("server:do_client_quit(...)~n"),
		M = maps:remove(ClientPID,State#serv_st.nicks),
		ChatNames = get_chatnames_by_clientpid(ClientPID,State),
		lists:map(fun (Id) ->
			ChatPid = get_chatpid_by_chatname(Id,State),
			ChatPid!{self(), Ref, unregister, ClientPID}
							end, ChatNames),
%%		print_map(M),
		N =  maps:map(fun(_,V) ->
			case lists:member(ClientPID,V) of
				true -> lists:delete(ClientPID,V);
				_ -> V
			end
			end,State#serv_st.registrations),
%%	print_map(N),
	ClientPID!{self(),Ref,ack_quit},
	next_state(M,N,State#serv_st.chatrooms).

next_state(Nicks,Regs,ChatRooms)->
	#serv_st{
		nicks = Nicks,
		registrations = Regs,
		chatrooms = ChatRooms
	}.

%%
%%print_map(Cons) ->
%%	maps:fold(
%%		fun(K, V, ok) ->
%%			io:format("Server ~p: ~p~n", [K, V])
%%		end, ok, Cons).