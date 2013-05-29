-module(simple_neuron).
-compile(export_all).

create() ->
  Weights = [random:uniform() - 0.5, random:uniform() - 0.5, random:uniform() - 0.5],
  register(neuron, spawn(?MODULE, loop, [Weights])).

loop(Weights) ->
  receive
    {From, Input} ->
      io:format("****Processing****~nInput:~p~nUsing Weights:~p~n", [Input, Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      From ! {result, Output},
      loop(Weights)
  end.

dot([I|Input],[W|Weights],Acc) ->
  dot(Input,Weights,I*W+Acc);
dot([],[Bias],Acc) ->
  Acc + Bias.

sense(Signal) ->
  case is_list(Signal) and (length(Signal) == 2) of
    true ->
      neuron ! {self(), Signal},
      receive
        {result, Output} ->
          io:format("Output: ~p~n", [Output])
      end;
    false ->
      io:format("The Signal must be a list of length 2~n")
  end.
