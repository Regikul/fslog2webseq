-module(fslog2webseq).

%% API exports
-export([main/1]).

-record(state, {
    a :: binary(),
    b :: binary(),
    message :: binary(),
    delim_count = 0 :: non_neg_integer(),
    now = skipping  :: skipping | wip | done
}).

while(Predicate, Function, State) ->
    While = fun
                (P, F, true, S0, Self) ->
                    S1 = F(S0),
                    Self(P, F, P(S1), S1, Self);
                (_P, _F, false, S, _Self) ->
                    S
            end,
    While(Predicate, Function, true, State, While).


%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    while(fun (X) -> X =/= stop end, fun step/1, nothing),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

step(_) ->
    case while(fun stil_parse/1, fun parse_line/1, #state{}) of
        #state{message = Message, a = A, b = B} ->
            io:format("~s -> ~s: ~s", [A, B, Message]),
            skip;
        stop -> stop
    end.

stil_parse(#state{now = Now}) ->
    case Now of
        skipping -> true;
        wip -> true;
        done -> false
    end;
stil_parse(stop) -> false.

block_start(<<"recv", _/binary>>, #state{now = skipping} = State) ->
    State#state{now = wip, b = <<"FreeSwitch">>};
block_start(<<"send", _/binary>>, #state{now = skipping} = State) ->
    State#state{now = wip, a = <<"FreeSwitch">>};
block_start(_, State) ->
    State.

first_delim(Line, #state{delim_count = 0, now = wip} = State) ->
    case binary:match(Line, <<"------">>) of
        {_, _} -> State#state{delim_count = 1};
        _ -> State
    end;
first_delim(_, State) ->
    State.

last_delim(Line, #state{delim_count = 1, now = wip} = State) ->
    case binary:match(Line, <<"------">>) of
        {_, _} -> State#state{delim_count = 2, now = done};
        _ -> State
    end;
last_delim(_, State) ->
    State.

catch_callid(Line, #state{now = wip} = State) ->
    case re:run(Line, <<"\s*Call-ID:\s*([\\S]+)">>, [{'capture', [1], 'binary'}]) of
        {match, [CallId]} -> set_call_id(CallId, State);
        nomatch -> State
    end;
catch_callid(_, State) ->
    State.

set_call_id(CallId, #state{a = undefined} = State) ->
    State#state{a = CallId};
set_call_id(CallId, #state{b = undefined} = State) ->
    State#state{b = CallId};
set_call_id(_, State) ->
    State.

catch_header(Line, #state{now = wip, message = undefined} = State) ->
    case binary:match(Line, <<"SIP/2.0">>) of
        {_, _} -> State#state{message = Line};
        _ -> State
    end;
catch_header(_, State) ->
    State.

parse_line(State0) ->
    Line = get_bin_str(),
    ParseFuns = [
        fun block_start/2,
        fun last_delim/2,
        fun first_delim/2,
        fun catch_callid/2,
        fun catch_header/2
    ],
    case Line =:= stop orelse lists:foldl(parse(Line), State0, ParseFuns) of
        true -> stop;
        State1 -> State1
    end.

parse(Line) ->
    fun(Fun, State) ->
        Fun(Line, State)
    end.

get_bin_str() ->
    case io:get_line([]) of
        {error, _Reason} ->
            io:format('standard_error', "can not read due to ~p~n", [_Reason]),
            stop;
        eof ->
            stop;
        Data ->
            list_to_binary(Data)
    end.
