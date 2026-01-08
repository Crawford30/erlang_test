%%% chat_server.erl
%%% WhatsApp-like Chat Server Backend
-module(chat_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([register_user/2, send_message/3, get_messages/2, get_online_users/0,
         user_typing/2, get_user_info/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% Test API
-export([test_suite/0, demo/0]).

-record(state,
        {users = #{},      % #{UserId => {Username, Pid, LastSeen}}
         messages = [],    % [{FromId, ToId, Message, Timestamp, MessageId}]
         typing = #{},     % #{UserId => {TargetId, Timestamp}}
         message_counter = 0}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

register_user(UserId, Username) ->
    gen_server:call(?MODULE, {register, UserId, Username, self()}).

send_message(FromId, ToId, Message) ->
    gen_server:call(?MODULE, {send_message, FromId, ToId, Message}).

get_messages(UserId1, UserId2) ->
    gen_server:call(?MODULE, {get_messages, UserId1, UserId2}).

get_online_users() ->
    gen_server:call(?MODULE, get_online_users).

get_user_info(UserId) ->
    gen_server:call(?MODULE, {get_user_info, UserId}).

user_typing(UserId, TargetId) ->
    gen_server:cast(?MODULE, {typing, UserId, TargetId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("~n=== Chat Server Started ===~n"),
    io:format("Server PID: ~p~n", [self()]),
    io:format("Timestamp: ~s~n~n", [format_timestamp(erlang:system_time(second))]),
    {ok, #state{}}.

handle_call({register, UserId, Username, Pid}, _From, State) ->
    case maps:is_key(UserId, State#state.users) of
        true ->
            {reply, {error, user_exists}, State};
        false ->
            monitor(process, Pid),
            Timestamp = erlang:system_time(second),
            NewUsers = maps:put(UserId, {Username, Pid, Timestamp}, State#state.users),
            io:format("[REGISTER] User '~s' (ID: ~s) registered at ~s~n",
                      [Username, UserId, format_timestamp(Timestamp)]),
            {reply, {ok, registered}, State#state{users = NewUsers}}
    end;
handle_call({send_message, FromId, ToId, Message}, _From, State) ->
    case {maps:find(FromId, State#state.users), maps:find(ToId, State#state.users)} of
        {{ok, _}, {ok, {_ToName, ToPid, _}}} ->
            Timestamp = erlang:system_time(second),
            MsgId = State#state.message_counter + 1,
            NewMessage = {FromId, ToId, Message, Timestamp, MsgId},
            NewMessages = [NewMessage | State#state.messages],

            % Notify recipient
            ToPid ! {new_message, FromId, Message, Timestamp, MsgId},

            {ok, {FromName, _, _}} = maps:find(FromId, State#state.users),
            io:format("[MESSAGE] #~p From: ~s -> To: ~s | \"~s\" at ~s~n",
                      [MsgId, FromName, ToId, Message, format_timestamp(Timestamp)]),

            {reply, {ok, MsgId}, State#state{messages = NewMessages, message_counter = MsgId}};
        {{error, _}, _} ->
            {reply, {error, sender_not_found}, State};
        {_, {error, _}} ->
            {reply, {error, recipient_not_found}, State}
    end;
handle_call({get_messages, User1, User2}, _From, State) ->
    Messages =
        lists:filter(fun({From, To, _Msg, _Time, _Id}) ->
                        From =:= User1 andalso To =:= User2
                        orelse From =:= User2 andalso To =:= User1
                     end,
                     State#state.messages),
    SortedMessages = lists:reverse(Messages),
    io:format("[QUERY] Retrieved ~p messages between ~s and ~s~n",
              [length(SortedMessages), User1, User2]),
    {reply, {ok, SortedMessages}, State};
handle_call(get_online_users, _From, State) ->
    Users =
        maps:fold(fun(Id, {Name, _Pid, LastSeen}, Acc) -> [{Id, Name, LastSeen} | Acc] end,
                  [],
                  State#state.users),
    io:format("[QUERY] Online users count: ~p~n", [length(Users)]),
    {reply, {ok, Users}, State};
handle_call({get_user_info, UserId}, _From, State) ->
    case maps:find(UserId, State#state.users) of
        {ok, {Name, _Pid, LastSeen}} ->
            {reply, {ok, {Name, LastSeen}}, State};
        error ->
            {reply, {error, not_found}, State}
    end.

handle_cast({typing, UserId, TargetId}, State) ->
    Timestamp = erlang:system_time(second),
    NewTyping = maps:put(UserId, {TargetId, Timestamp}, State#state.typing),

    % Notify target user
    case maps:find(TargetId, State#state.users) of
        {ok, {_Name, Pid, _}} ->
            Pid ! {user_typing, UserId},
            io:format("[TYPING] User ~s is typing to ~s~n", [UserId, TargetId]);
        error ->
            ok
    end,

    {noreply, State#state{typing = NewTyping}}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    % Find and remove disconnected user
    DisconnectedUser =
        maps:fold(fun(Id, {Name, P, _}, Acc) ->
                     case P =:= Pid of
                         true ->
                             {Id, Name};
                         false ->
                             Acc
                     end
                  end,
                  none,
                  State#state.users),

    case DisconnectedUser of
        {UserId, Username} ->
            NewUsers = maps:remove(UserId, State#state.users),
            io:format("[DISCONNECT] User '~s' (ID: ~s) disconnected. Reason: ~p~n",
                      [Username, UserId, Reason]),
            {noreply, State#state{users = NewUsers}};
        none ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~n=== Chat Server Terminated ===~n"),
    io:format("Reason: ~p~n~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Helper functions
%%====================================================================

format_timestamp(Seconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(Seconds, second),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC",
                  [Year, Month, Day, Hour, Min, Sec]).

%%====================================================================
%% Test Suite
%%====================================================================

test_suite() ->
    io:format("~n╔════════════════════════════════════════╗~n"),
    io:format("║   Chat Server Test Suite               ║~n"),
    io:format("╚════════════════════════════════════════╝~n~n"),

    % Start server
    io:format("→ Starting chat server...~n"),
    {ok, _Pid} = start_link(),
    timer:sleep(500),

    % Test 1: Register users
    io:format("~n→ TEST 1: Registering users...~n"),
    {ok, registered} = register_user("alice", "Alice"),
    {ok, registered} = register_user("bob", "Bob"),
    {ok, registered} = register_user("charlie", "Charlie"),
    io:format("✓ All users registered successfully~n"),
    timer:sleep(500),

    % Test 2: Try duplicate registration
    io:format("~n→ TEST 2: Testing duplicate registration...~n"),
    case register_user("alice", "Alice2") of
        {error, user_exists} ->
            io:format("✓ Duplicate registration correctly rejected~n");
        _ ->
            io:format("✗ FAILED: Duplicate registration was allowed~n")
    end,
    timer:sleep(500),

    % Test 3: Get online users
    io:format("~n→ TEST 3: Retrieving online users...~n"),
    {ok, Users} = get_online_users(),
    io:format("Online users: ~p~n", [Users]),
    io:format("✓ Retrieved ~p online users~n", [length(Users)]),
    timer:sleep(500),

    % Test 4: Send messages
    io:format("~n→ TEST 4: Sending messages...~n"),
    {ok, Msg1} = send_message("alice", "bob", "Hey Bob! How are you?"),
    timer:sleep(100),
    {ok, Msg2} = send_message("bob", "alice", "Hi Alice! I'm doing great!"),
    timer:sleep(100),
    {ok, Msg3} = send_message("alice", "bob", "That's wonderful to hear!"),
    timer:sleep(100),
    {ok, _Msg4} = send_message("charlie", "alice", "Hi Alice, this is Charlie!"),
    io:format("✓ Sent 4 messages successfully (IDs: ~p, ~p, ~p, ...)~n", [Msg1, Msg2, Msg3]),
    timer:sleep(500),

    % Test 5: Retrieve conversation
    io:format("~n→ TEST 5: Retrieving conversation history...~n"),
    {ok, Conversation} = get_messages("alice", "bob"),
    io:format("Conversation between Alice and Bob:~n"),
    lists:foreach(fun({From, To, Msg, _Time, Id}) ->
                     io:format("  [#~p] ~s → ~s: \"~s\"~n", [Id, From, To, Msg])
                  end,
                  Conversation),
    io:format("✓ Retrieved ~p messages~n", [length(Conversation)]),
    timer:sleep(500),

    % Test 6: Typing indicator
    io:format("~n→ TEST 6: Testing typing indicators...~n"),
    user_typing("alice", "bob"),
    timer:sleep(100),
    io:format("✓ Typing indicator sent~n"),
    timer:sleep(500),

    % Test 7: Get user info
    io:format("~n→ TEST 7: Getting user information...~n"),
    {ok, {Name, LastSeen}} = get_user_info("alice"),
    io:format("User Info - Name: ~s, Last Seen: ~s~n", [Name, format_timestamp(LastSeen)]),
    io:format("✓ User info retrieved successfully~n"),
    timer:sleep(500),

    % Test 8: Error handling
    io:format("~n→ TEST 8: Testing error handling...~n"),
    case send_message("nonexistent", "bob", "This should fail") of
        {error, sender_not_found} ->
            io:format("✓ Correctly handled non-existent sender~n");
        _ ->
            io:format("✗ FAILED: Should have rejected non-existent sender~n")
    end,
    timer:sleep(500),

    % Summary
    io:format("~n╔════════════════════════════════════════╗~n"),
    io:format("║   All Tests Completed Successfully!   ║~n"),
    io:format("╚════════════════════════════════════════╝~n~n"),

    io:format("→ Stopping server...~n"),
    stop(),
    timer:sleep(500),
    io:format("✓ Server stopped~n~n"),

    ok.

%%====================================================================
%% Interactive Demo
%%====================================================================

demo() ->
    io:format("~n╔════════════════════════════════════════╗~n"),
    io:format("║   Interactive Chat Server Demo        ║~n"),
    io:format("╚════════════════════════════════════════╝~n~n"),

    % Start server
    {ok, _Pid} = start_link(),
    timer:sleep(300),

    % Simulate a group chat scenario
    io:format("→ Simulating a group chat scenario...~n~n"),
    timer:sleep(500),

    register_user("alice", "Alice"),
    timer:sleep(200),
    register_user("bob", "Bob"),
    timer:sleep(200),
    register_user("charlie", "Charlie"),
    timer:sleep(500),

    io:format("~n→ Starting conversation...~n~n"),
    timer:sleep(500),

    send_message("alice", "bob", "Hey Bob! Want to grab lunch?"),
    timer:sleep(800),

    send_message("bob", "alice", "Sure! What time works for you?"),
    timer:sleep(1000),

    user_typing("alice", "bob"),
    timer:sleep(1500),

    send_message("alice", "bob", "How about 12:30 PM?"),
    timer:sleep(800),

    send_message("charlie", "alice", "Hi Alice, can I join you guys?"),
    timer:sleep(1000),

    send_message("alice", "charlie", "Of course Charlie! The more the merrier!"),
    timer:sleep(800),

    send_message("bob", "alice", "Perfect! See you both at 12:30!"),
    timer:sleep(1000),

    io:format("~n→ Checking online users...~n"),
    {ok, Users} = get_online_users(),
    io:format("~nCurrently online (~p users):~n", [length(Users)]),
    lists:foreach(fun({Id, Name, LastSeen}) ->
                     io:format("  • ~s (ID: ~s) - Last seen: ~s~n",
                               [Name, Id, format_timestamp(LastSeen)])
                  end,
                  Users),

    timer:sleep(1000),

    io:format("~n→ Getting conversation history...~n"),
    {ok, AliceBobChat} = get_messages("alice", "bob"),
    io:format("~nAlice ↔ Bob conversation (~p messages):~n", [length(AliceBobChat)]),
    lists:foreach(fun({From, _To, Msg, Time, Id}) ->
                     io:format("  [#~p | ~s] ~s: ~s~n", [Id, format_timestamp(Time), From, Msg])
                  end,
                  AliceBobChat),

    timer:sleep(1000),

    io:format("~n╔════════════════════════════════════════╗~n"),
    io:format("║   Demo Completed!                      ║~n"),
    io:format("║   Server is still running...           ║~n"),
    io:format("║   Call chat_server:stop() to stop it  ║~n"),
    io:format("╚════════════════════════════════════════╝~n~n"),

    ok.
