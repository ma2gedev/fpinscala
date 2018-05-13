# implement without pattern match
defmodule GettingStarted do
  # exercise 2-1

  # exercise 2-2
  # iex(5)> GettingStarted.is_sorted([1,2,3,5], fn x, y -> x < y end)
  # true
  # iex(6)> GettingStarted.is_sorted([1,2,3,5,4], fn x, y -> x < y end)
  # false
  # iex(7)> GettingStarted.is_sorted([1,2,3,5,4], fn x, y -> x < y end)
  # false
  def is_sorted(list, ordered) do
    sorted?(list, ordered, 0)
  end

  defp sorted?(list, ordered, n) do
    cond do
      Enum.count(list) - 1 <= n -> true
      !ordered.(Enum.at(list, n), Enum.at(list, n + 1)) -> false
      true -> sorted?(list, ordered, n + 1)
    end
  end

  # exercise 2-3
  # iex(9)> fn1 = GettingStarted.curry(fn x, y -> x + y end)
  # #Function<0.30686482/1 in GettingStarted.curry/1>
  # iex(10)> fn1.(1)
  # #Function<1.30686482/1 in GettingStarted.curry/1>
  # iex(11)> fn1.(1).(2)
  # 3
  def curry(fun) do
    fn(x) ->
      fn(y) ->
        fun.(x, y)
      end
    end
  end

  # exercise 2-4
  # iex(16)> fn1 = GettingStarted.curry(fn x, y -> x + y end)
  # #Function<0.15797096/1 in GettingStarted.curry/1>
  # iex(17)> fn2 = GettingStarted.uncurry(fn1)
  # #Function<1.15797096/2 in GettingStarted.uncurry/1>
  # iex(18)> fn2.(1,2)
  # 3
  def uncurry(fun) do
    fn(x, y) ->
      fun.(x).(y)
    end
  end

  # exercise 2-5
  # iex(20)> fu = GettingStarted.compose(fn x -> x + 4 end, fn y -> y + 1 end)
  # Function<0.73114054/1 in GettingStarted.compose/2>
  # iex(21)> fu.(1)
  def compose(f, g) do
    fn(a) ->
      f.(g.(a))
    end
  end
end
