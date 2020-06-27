(module

(func $unbox_integer (param $a (ref $i31)) (result i32)
  (local.get $a)
  (struct.get $i31 $value)
)

(func $box_integer (param $x i32) (result (ref $i31))
  (local.get $x)
  (struct.new $i31)
)


(type $i31 (struct (field $value i32)))
(type $value (struct (field $header i32) (field $contents (ref $block_contents))))

(type $block_contents (array anyref))

(func (export "caml_alloc") (param $size i32) (param $tag i32) (result anyref)
;; TODO: implement something that calls a javascript runtime that polyfills the GC proposal or use experimental GC support e.g. from V8
  ref.null
  )

(func (export "caml_obj_dup") (param $obj anyref) (result anyref)
  ref.null
  )
)