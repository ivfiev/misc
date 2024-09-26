defmodule Ch1 do
  def f do
    f = fn x -> x * x end

    y = fn
      y when is_number(y) -> :a
      _ -> :b
    end

    f.(5) + (y && 5)
  end

  def e do
    5.0 === 5
  end
end

defmodule Mapz do
  def m do
    m = %{a: "b", c: 9}
    m
  end
end

defmodule Patterns do
  def p do
    {a, a, a} = {1, 1, 1}
    a + a
  end

  def m(1) do
    1
  end

  def m(2) do
    3
  end

  def m(x) when x > 2 do
    5
  end
end

defmodule Fib do
  # def calc(n) when n === -12.3, do: if(34 > 54, do: :fib)
  def calc(n) when n <= 1, do: n
  def calc(n), do: calc(n - 1) + calc(n - 2)

  def par(m, n) do
    Enum.map(1..m, fn _ -> spawn(fn -> IO.puts(calc(n)) end) end)
  end
end

defmodule With do
  def w do
    with {:a, 1} <- {:a, 1},
         {:b, 2} <- {:c, 3} do
      5
    end
  end
end

defmodule FibServer do
  defp initial_state, do: %{}

  def start() do
    spawn(fn -> loop(initial_state()) end)
  end

  defp loop(state) do
    receive do
      {:fib, n, ret} ->
        if Map.has_key?(state, n) do
          send(ret, {:fib_result, Map.get(state, n)})
          loop(state)
        else
          result = Fib.calc(n)
          send(ret, {:fib_result, result})
          new_state = Map.put(state, n, result)
          loop(new_state)
        end
    end
  end

  def calc(pid, n) do
    send(pid, {:fib, n, self()})

    receive do
      {:fib_result, result} ->
        IO.puts(result)
        result
    end
  end
end

defmodule GenericServer do
  def start(module, initial_state) do
    spawn(fn -> loop(module, initial_state) end)
  end

  def call(pid, request) do
    send(pid, {:call, request, self()})

    receive do
      {:response, response} ->
        response
    end
  end

  def cast(pid, request) do
    send(pid, {:cast, request})
  end

  defp loop(module, state) do
    receive do
      {:call, request, sender} ->
        {new_state, response} = module.handle_call(state, request)
        send(sender, {:response, response})
        loop(module, new_state)

      {:cast, request} ->
        new_state = module.handle_cast(state, request)
        loop(module, new_state)

      _ ->
        loop(module, state)
    end
  end
end

defmodule KVServer do
  def start() do
    GenericServer.start(KVServer, %{})
  end

  def get(pid, key) do
    GenericServer.call(pid, {:get, key})
  end

  def put(pid, key, val) do
    GenericServer.cast(pid, {:put, key, val})
  end

  def handle_call(kv, {:get, key}) do
    {kv, Map.get(kv, key)}
  end

  def handle_cast(kv, {:put, key, val}) do
    Map.put(kv, key, val)
  end
end
