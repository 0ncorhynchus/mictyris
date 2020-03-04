use super::*;
use std::cell::RefCell;
use std::rc::Rc;

pub fn extends(root: &Env, args: &[String], locations: &[Location]) -> Env {
    let mut env = Environment::make_scope(Rc::clone(root));
    let itr = args.iter().zip(locations.iter().cloned());
    for (ident, location) in itr {
        env.insert(ident, location);
    }
    Rc::new(RefCell::new(env))
}

pub fn wrong(message: &'static str) -> CommCont {
    let err = EvalError {
        message: message.to_string(),
    };
    Rc::new(move |_| Err(err.clone()))
}

pub fn send(value: Value, cont: ExprCont) -> CommCont {
    cont(vec![value])
}

pub fn single<F: 'static>(f: F) -> ExprCont
where
    F: Fn(Value) -> CommCont,
{
    Rc::new(move |mut values| {
        if values.len() == 1 {
            f(values.pop().unwrap())
        } else {
            wrong("wrong number of return values")
        }
    })
}

pub fn hold(location: Location, cont: ExprCont) -> CommCont {
    Rc::new(move |store: &mut Store| {
        let cont = send(store.get(&location), Rc::clone(&cont));
        cont(store)
    })
}

pub fn assign(location: Location, value: Value, cont: CommCont) -> CommCont {
    Rc::new(move |store: &mut Store| {
        store.update(&location, value.clone());
        cont(store)
    })
}

pub fn tievals(f: Rc<dyn Fn(&[Location]) -> CommCont>, values: &[Value]) -> CommCont {
    match values.split_first() {
        Some((head, tail)) => {
            let head = head.clone();
            let tail = tail.to_vec();
            let f = Rc::clone(&f);
            Rc::new(move |store: &mut Store| {
                let location = store.reserve();
                let loc = location.clone();
                let f = Rc::clone(&f);
                let new_f = Rc::new(move |locations: &[Location]| {
                    let mut new_locs = Vec::with_capacity(locations.len() + 1);
                    new_locs.push(loc.clone());
                    new_locs.extend_from_slice(locations);
                    f(&new_locs)
                });

                store.update(&location, head.clone());
                tievals(new_f, &tail)(store)
            })
        }
        None => f(&[]),
    }
}

pub fn tievalsrest(f: Rc<dyn Fn(&[Location]) -> CommCont>, values: &[Value], n: usize) -> CommCont {
    let rest = values[..n].to_vec();
    list(
        &values[n..],
        single(move |value| {
            let mut rest = rest.clone();
            rest.push(value);
            tievals(Rc::clone(&f), &rest)
        }),
    )
}

pub fn truish(value: Value) -> bool {
    value != Bool(false)
}

pub fn applicate(f: &Value, args: &[Value], cont: ExprCont) -> CommCont {
    match f {
        Procedure(proc) => (proc.inner)(args, cont),
        _ => wrong("bad procedure"),
    }
}

pub fn twoarg<F: 'static>(f: F, values: &[Value], cont: ExprCont) -> CommCont
where
    F: Fn(&Value, &Value, ExprCont) -> CommCont,
{
    match values {
        [arg1, arg2] => f(arg1, arg2, cont),
        _ => wrong("wrong number of arguments"),
    }
}
