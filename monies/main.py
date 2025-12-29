import scipy.integrate
import scipy.optimize


def derivatives(t, y, rate, payment, appreciation):
    [price, owed, interest] = y
    return [price * appreciation, rate * owed - payment, rate * owed]


def solve_ivp(f, y0, t0t1, consts):
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


def p(str, x):
    print(f"{str} {round(x, 2)}")


if __name__ == "__main__":
    years = 30
    rate = 0.05
    house_price = 100
    house_appreciation = 0.03

    continuous_annual_payment = solve_root(
        lambda x: solve_ivp(
            derivatives,
            [house_price, house_price, 0],
            (0, years),
            (rate, x, house_appreciation),
        )(years)[1],
        (0, 10000),
    )
    f = solve_ivp(
        derivatives,
        [house_price, house_price, 0],
        (0, years),
        (rate, continuous_annual_payment, house_appreciation),
    )

    ys = f(years)
    p("current house price:", ys[0])
    p("owed amount remaining:", ys[1])
    p("total interest paid:", ys[2])
    p("total paid:", house_price + ys[2])
    p("profit:", ys[0] - house_price - ys[2])

    # maintenance + insurance costs + property tax
    # compare with renting
    # opportunity cost on down-payment
    # selling & capital gains
    # compare against discrete process?
