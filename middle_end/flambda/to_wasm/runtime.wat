(module

(type $block_contents (array anyref))
(type $value (struct (field $header i32) (field $contents anyref)))

(func (export "caml_alloc") (param $size i32) (param $tag i32) (result anyref)
;; TODO: implement something that calls a javascript runtime that polyfills the GC proposal or use experimental GC support e.g. from V8
  ref.null
  )

(func (export "caml_obj_dup") (param $obj anyref) (result anyref)
  
  )
)