use std::sync::RwLock;

use rustler::{NifMap, NifTuple, NifUnitEnum, NifTaggedEnum, NifUntaggedEnum, ResourceArc};
use rustler::{NifResult, LocalPid, Encoder, Atom, Env, OwnedEnv, Term};
use rustler::types::atom::ok;

// See Derive Macros docs at https://docs.rs/rustler/0.26.0/rustler/index.html#derives
mod atoms{
    rustler::atoms!{
        task_finished,
        runtime_already_stop
    }
}
#[derive(NifMap)]
struct MyMap {
    lhs: i32,
    rhs: i32,
}

#[derive(NifTuple)]
struct MyTuple {
    lhs: i32,
    rhs: i32,
}

#[derive(NifUnitEnum)]
enum UnitEnum {
    FooBar,
    Baz,
}

#[derive(NifTaggedEnum)]
enum TaggedEnum {
    Foo,
    Bar(String),
    Baz{ a: i32, b: i32 },
}

#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
}

#[rustler::nif(name = "add")]
fn add_nif(a: i64, b: i64) -> i64 {
    add(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif(name = "my_map")]
fn my_map_nif() -> MyMap {
    my_map()
}

#[rustler::nif]
fn my_maps() -> Vec<MyMap> {
    vec![ my_map(), my_map()]
}

fn my_map() -> MyMap {
    MyMap { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn my_tuple() -> MyTuple {
    MyTuple { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
    unit_enum
}

#[rustler::nif]
fn tagged_enum_echo(tagged_enum: TaggedEnum) -> TaggedEnum {
    tagged_enum
}

#[rustler::nif]
fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
    untagged_enum
}
#[rustler::nif]
fn async_task(pid: LocalPid) -> NifResult<Atom>{
     std::thread::spawn(move || {
          // do something
          let mut env = OwnedEnv::new();  
          env.send_and_clear(&pid, |env|{
            crate::atoms::task_finished().encode(env)
          });
     }); 
     Ok(ok())
}
pub struct RunTimeRes{
    handle: tokio::runtime::Handle,
    stop_tx: RwLock<Option<tokio::sync::oneshot::Sender<()>>>
}
fn load(env: Env, _info: Term) -> bool{
     rustler::resource!(RunTimeRes, env);
     rustler::resource!(Actor, env);
     true
}
#[rustler::nif]
fn start_runtime(pid: LocalPid) -> ResourceArc<RunTimeRes>{
    let (stop_tx, stop_rx) = tokio::sync::oneshot::channel();
    let (handle_tx, handle_rx) = std::sync::mpsc::channel();
    std::thread::spawn(move ||{
       let runtime = tokio::runtime::Builder::new_multi_thread().enable_all().build().unwrap(); 
       handle_tx.send(runtime.handle().clone()).unwrap();
       runtime.block_on(stop_rx).unwrap();
       OwnedEnv::new().send_and_clear(&pid, |env|{
         atoms::runtime_already_stop().encode(env) 
       });
    });
    let handle = handle_rx.recv().unwrap();
    ResourceArc::new(
        RunTimeRes { 
            handle, 
            stop_tx: RwLock::new(Some(stop_tx))
        }
    )
}

#[rustler::nif]
fn stop_runtime(runtime: ResourceArc<RunTimeRes>) -> NifResult<Atom>{
    if let Some(stop_rx)  = runtime.stop_tx.write().unwrap().take(){
        let _ = stop_rx.send(());
        Ok(ok())
    }else{
        Err(rustler::Error::BadArg)
    }
}
pub struct Actor{
    runtime: ResourceArc<RunTimeRes>,
    send_tx: RwLock<Option<tokio::sync::mpsc::Sender<(LocalPid, String)>>>
}
async fn loop_actor(mut send_rx: tokio::sync::mpsc::Receiver<(LocalPid, String)>){
  loop {
      tokio::select! {
            Some((pid, msg)) = send_rx.recv() =>{
                // do something
                OwnedEnv::new().send_and_clear(&pid, |env|{
                     msg.encode(env)
                });
            }
            else =>{
                break;
            }
      }
  }
}
#[rustler::nif]
fn start_actor(runtime: ResourceArc<RunTimeRes>) -> ResourceArc<Actor>{
  let (send_tx, send_rx) = tokio::sync::mpsc::channel(128);
  runtime.handle.spawn(loop_actor(send_rx)); 
  ResourceArc::new(Actor {
     runtime,
     send_tx: RwLock::new(Some(send_tx))
    })
}
#[rustler::nif]
fn to_actor(actor: ResourceArc<Actor>, pid: LocalPid, msg: String) -> NifResult<Atom>{
      let send_tx = actor.send_tx.write().unwrap().clone();
       if send_tx.is_none(){
        return Err(rustler::Error::BadArg);
       }
       actor.runtime.handle.spawn(async move {
          let _ = send_tx.unwrap().send((pid, msg)).await;
       });
       Ok(ok())
}
#[rustler::nif]
fn stop_actor(actor: ResourceArc<Actor>) -> NifResult<Atom>{
    if let Some(send_tx) = actor.send_tx.write().unwrap().take(){
            drop(send_tx);
    }
    Ok(ok())
}
rustler::init!("rust_conn",
    [ add_nif
    , my_map_nif
    , my_maps
    , my_tuple
    , unit_enum_echo
    , tagged_enum_echo
    , untagged_enum_echo
    , async_task
    , start_runtime
    , stop_runtime
    , start_actor
    , to_actor
    , stop_actor
    ],
    load = load
);

#[cfg(test)]
mod tests {
    use crate::add;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
