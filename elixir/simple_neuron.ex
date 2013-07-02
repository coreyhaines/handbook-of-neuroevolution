defmodule SimpleNeuron do
  def create do
    [:random.uniform() - 0.5, :random.uniform() - 0.5, :random.uniform() - 0.5]
      |> (fn x -> Process.spawn(__MODULE__, :loop, [x]) end).()
      |> Process.register(:neuron)
  end

  def dot([i|input], [w|weights], acc) do
    dot(input,weights,i*w+acc)
  end
  def dot([],[bias],acc) do
    acc + bias
  end

  def loop(weights) do
    receive do
      {from, input} ->
        :io.format("****Processing****~nInput:~p~nUsing Weights:~p~n", [input, weights])
        output = dot(input, weights, 0) |> :math.tanh
        from <- {:result, [output]}
        loop(weights)
    end
  end

  def send(signal) do
    case is_list(signal) and (length(signal) == 2) do
      true ->
        :neuron <- {self(), signal}
        receive do
          {:result, output} ->
            :io.format("Output: ~p~n", [output])
        end
      false ->
        :io.format("The Signal must a list of length 2~n")
    end
  end
end
