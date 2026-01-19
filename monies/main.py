import math
import scipy.integrate
import scipy.optimize


def mortgage_ds(t, y, rate, payment, appreciation, expense_rate):
    price, owed, interest, expenses = y
    return [
        price * appreciation,
        rate * owed - payment,
        rate * owed,
        price * expense_rate,
    ]


def rent_ds(t, y, rent_rate, capital_rate):
    total, annual_payment, capital = y
    return [annual_payment, annual_payment * rent_rate, capital * capital_rate]


def solve_ivp(f, y0, t0t1, consts=()):
    soln = scipy.integrate.solve_ivp(
        fun=f,
        y0=y0,
        t_span=t0t1,
        args=consts,
        dense_output=True,
    ).sol
    return lambda x: soln(x)


def solve_root(f, ab) -> float:
    return scipy.optimize.brentq(f, ab[0], ab[1])  # pyright: ignore[reportReturnType]


def p(label, x):
    print(f"{label:<32} {x:,.2f}")


def annual_expenses():
    maintenance = 0.01
    insurance = 0.001
    tax = 0.003
    return maintenance + insurance + tax


if __name__ == "__main__":
    years = 25
    rate = 0.05
    house_price = 500000
    house_appreciation = 0.03
    expenses = annual_expenses()
    rent_rate = house_appreciation - 0.005
    annual_rent_cost = 24000

    ivp = lambda continuous_annual_payment: solve_ivp(
        mortgage_ds,
        [house_price, house_price, 0, 0],
        (0, years),
        (rate, continuous_annual_payment, house_appreciation, expenses),
    )

    continuous_annual_payment = solve_root(
        lambda x: ivp(x)(years)[1],
        (0, 1000000),
    )
    f = ivp(continuous_annual_payment)
    g = solve_ivp(
        rent_ds,
        [0, annual_rent_cost, 0.15 * house_price],
        (0, years),
        (rent_rate, rate),
    )

    for year in [5, 10, 20, years]:
        print("--")
        print(f"year: {year}")
        print("--")
        new_house_price, owed_remaining, interest_paid, expenses_paid = f(year)
        total_paid = house_price + interest_paid + expenses_paid
        p("approx monthly payment at the beginning:", continuous_annual_payment / 12.0)
        p("current house price:", new_house_price)
        p("owed amount remaining:", owed_remaining)
        p("total interest paid:", interest_paid)
        p("total paid:", total_paid)
        p("profit:", new_house_price - total_paid)
        p("expenses:", expenses_paid)
        print("--")
        total_paid, new_rent, new_capital = g(year)
        p("total paid in rent:", total_paid)
        p("original rent monthly:", annual_rent_cost / 12.0)
        p("current rent monthly:", new_rent / 12.0)
        p("capital gained:", new_capital)
        print("--")
