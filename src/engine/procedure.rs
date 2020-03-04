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
        None => send(Null, cont),
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
                send(Pair(loc1, loc2, true), Rc::clone(&cont))(store)
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
            send(Bool(lhs < rhs), Rc::clone(&cont))
        },
        values,
        cont,
    )
}
