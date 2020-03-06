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

pub fn apply(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(
        |operator, operand, cont| match operator {
            Procedure(proc) => {
                let proc = Rc::clone(&proc.inner);
                valueslist(
                    &[operand.clone()],
                    Rc::new(move |values| proc(&values, Rc::clone(&cont))),
                )
            }
            _ => wrong("bad procedure argument"),
        },
        values,
        cont,
    )
}

// support function
pub fn valueslist(values: &[Value], cont: ExprCont) -> CommCont {
    onearg(
        |arg, cont| match arg {
            Pair(_, _, _) => {
                let arg = arg.clone();
                let cont = cont.clone();
                cdr(
                    &[arg.clone()],
                    Rc::new(move |values| {
                        let arg = arg.clone();
                        let cont = cont.clone();
                        valueslist(
                            &values,
                            Rc::new(move |values| {
                                let cont = cont.clone();
                                car(
                                    &[arg.clone()],
                                    single(move |value| {
                                        let mut values = values.clone();
                                        values.insert(0, value);
                                        cont(values)
                                    }),
                                )
                            }),
                        )
                    }),
                )
            }
            Null => cont(vec![]),
            _ => wrong("non-list argument"),
        },
        values,
        cont,
    )
}

pub fn cwcc(values: &[Value], cont: ExprCont) -> CommCont {
    onearg(
        |arg, cont| match arg {
            Procedure(proc) => {
                let proc = Rc::clone(&proc.inner);
                let cont = Rc::clone(&cont);
                Rc::new(move |store| {
                    let new_cont = Rc::clone(&cont);
                    let new_proc = Procedure(Proc::new(Rc::new(move |values, _cont| {
                        new_cont(values.to_vec())
                    })));
                    proc(&[new_proc], Rc::clone(&cont))(store)
                })
            }
            _ => wrong("bad procedure argument"),
        },
        values,
        cont,
    )
}

pub fn values(values: &[Value], cont: ExprCont) -> CommCont {
    cont(values.to_vec())
}

pub fn cwv(values: &[Value], cont: ExprCont) -> CommCont {
    twoarg(
        |lhs, rhs, cont| {
            let rhs = rhs.clone();
            let cont = Rc::clone(&cont);
            applicate(
                &lhs,
                &[],
                Rc::new(move |values| applicate(&rhs, &values, Rc::clone(&cont))),
            )
        },
        values,
        cont,
    )
}
