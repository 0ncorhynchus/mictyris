use super::*;

pub fn list(values: &[Value], cont: ExprCont) -> CommCont {
    match values.split_first() {
        Some((head, tail)) => {
            let head = head.clone();
            list(
                tail,
                single(move |value| cons(&[head.clone(), value], Rc::clone(&cont))),
            )
        }
        None => send(Null, &cont),
    }
}

pub fn cons(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(
        |head, tail, cont| {
            let head = head.clone();
            let tail = tail.clone();
            Rc::new(move |store: &mut Store| {
                let loc1 = store.reserve();
                store.update(&loc1, head.clone());
                let loc2 = store.reserve();
                store.update(&loc2, tail.clone());
                send(Pair(loc1, loc2, true), &cont)(store)
            })
        },
        values,
        cont,
    )
}

pub fn less(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(
        |lhs, rhs, cont| {
            let lhs = match lhs.number() {
                Some(num) => num,
                None => return wrong("non-numeric argument"),
            };
            let rhs = match rhs.number() {
                Some(num) => num,
                None => return wrong("non-numeric argument"),
            };
            send(Bool(lhs < rhs), &cont)
        },
        values,
        cont,
    )
}

pub fn add(values: &[Value], cont: ExprCont) -> CommCont {
    let mut args = Vec::with_capacity(values.len());
    for v in values {
        match v.number() {
            Some(num) => args.push(num),
            None => return wrong("non-numeric argument"),
        }
    }
    send(Number(args.into_iter().sum()), &cont)
}

pub fn car(values: &[Value], cont: ExprCont) -> CommCont {
    onearg(
        |arg, cont| match arg.pair() {
            Some((car, _, _)) => hold(car, cont),
            None => wrong("non-pair argument"),
        },
        values,
        cont,
    )
}

pub fn cdr(values: &[Value], cont: ExprCont) -> CommCont {
    onearg(
        |arg, cont| match arg.pair() {
            Some((_, cdr, _)) => hold(cdr, cont),
            None => wrong("non-pair argument"),
        },
        values,
        cont,
    )
}

pub fn setcar(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(
        |pair, value, cont| match pair.pair() {
            Some((car, _, mutable)) => {
                if mutable {
                    assign(car, value.clone(), send(Unspecified, &cont))
                } else {
                    wrong("immutable argument")
                }
            }
            None => wrong("non-pair argument"),
        },
        values,
        cont,
    )
}

pub fn eqv(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(|lhs, rhs, cont| send(Bool(lhs == rhs), &cont), values, cont)
}
