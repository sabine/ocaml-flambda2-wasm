(module

;; In Flambda there is a primitive Is_int that we need to implement. This is coming from
;; variant types which are encoded differently, depending on whether the tag
;; has values attached (-> heap block with the variant tag and values)
;; or not (-> unboxed integer represents the variant tag)
;; The primitive Is_int is usually compiled
;; to code that checks the tag bit in order to determine whether a heap block field is an int or a pointer.
;; here, to inject i32 as fields of a heap block, in a way that we know whether the thing is an integer value,
;; we pack every i32 in a heap block and that has a field that points to another heap struct that holds only the i32
;; so, integers aren't just boxed, they are double boxed

;; obj.ml line 52: let last_non_constant_constructor_tag = 245
;; This is the last tag that would normally be assigned to a variant type. We hope no one uses such huge
;; variant types, as we "steal" this last tag to represent a "boxed unboxed integer"
(global $boxed_unboxed_integer_tag i32 (i32.const 245))

(type $boxed_unboxed_integer (struct (field $value i32)))
(type $value (struct (field $tag i32) (field $contents (ref $block_contents))))
(type $block_contents (array (mut anyref)))

(func $is_int (param $x (ref $value)) (result i32)
  (local.get $x)
  (struct.get $value $tag)
  (global.get $boxed_unboxed_integer_tag)
  (i32.eq)
)

(func $unbox_boxed_unboxed_integer (param $a (ref $value)) (result i32)
  (local.get $a)
  (struct.get $value $contents)
  (i32.const 0)
  (array.get $block_contents)
  ;; TODO: ref.cast isn't implemented in wabt
  (ref.cast anyref $boxed_unboxed_integer)
  (struct.get $boxed_unboxed_integer $value)
)

(func $box_boxed_unboxed_integer (param $x i32) (result (ref $value))
  (global.get $boxed_unboxed_integer_tag)

  ;; box
  (local.get $x)
  (struct.new $boxed_unboxed_integer)

  ;; make an array of length 1
  (i32.const 1)
  (array.new $block_contents)

  (i32.const 0)
  (array.set $block_contents)

  (struct.new $value)
  

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