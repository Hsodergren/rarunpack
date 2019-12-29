type 'a t = {
  data: 'a;
  updatefn: 'a -> Progress.t -> 'a;
  printfn: 'a -> unit
}

let create data updatefn printfn = {data; updatefn; printfn}
    
let update t p = {t with data = (t.updatefn t.data p)}

let print t = t.printfn t.data

