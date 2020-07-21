(module


(type $value (struct (field $tag i32) (field $contents (ref $block_contents))))
(type $block_contents (array (mut anyref)))

(func $is_int (param $x anyref) (result i32)
  (local.get $x)
  (rtt.canon i31)
  (ref.test)
)

(func $tag_integer (param $a anyref) (result i32)
  (local.get $a)
  (rtt.canon i31)
  (ref.cast)
  (i31.get_u)
)

(func $untag_integer (param $x i32) (result anyref)
  (local.get $x)
  (i31.new)
  (rtt.canon any)
  (ref.cast)
)


(func $caml_alloc (export "caml_alloc") (param $size i32) (param $tag i32) (result (ref $value))
;; create a $value struct with the given $tag and an anyref array of size $size with null references
  (local.get $tag)

  (ref.null)
  (local.get $size)
  (array.new $block_contents)
  
  (struct.new $value)
  )

(func $caml_obj_dup (export "caml_obj_dup") (param $obj (ref $value)) (result (ref $value))
  (local $new_obj (ref $value))
  (local $i i32)
  (local $new_array (ref $block_contents))
  (local $array (ref $block_contents))

  (local.get $obj)
  (struct.get $value $contents)
  (local.set $array)

  (local.get $obj)
  (struct.get $value $tag)

  (local.get $obj)
  (struct.get $value $contents)
  (array.len $block_contents)

  (call $caml_alloc)
  (local.set $new_obj)

  (local.get $new_obj)
  (struct.get $value $contents)
  (local.set $new_array)

  (i32.const 0)
  (local.set $i)
  (loop

    (local.get $new_array)
    (local.get $i)

    (local.get $array)
    (local.get $i)
    (array.get $block_contents)

    (array.set $block_contents)

    ;; increment array index i
    (local.get $i)
    (i32.const 1)
    (i32.add)
    (local.set $i)

    ;;(return)
    )

;; TODO: copy the contents over, and allocate sub-structures recursively
  (local.get $new_obj)
  )
)
