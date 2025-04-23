(* 
              Gray scale image processing demonstration

Alternative implementation using arrays rather than lists. To use,
rename to `image.ml`.

See `image.mli` for more complete documentation.
 *)

module G = Graphics ;;
let cMAXRGB = 255 ;;    (* maximum RGB channel value in Graphics module *)
  
(* Pixels are floats in the range [0..1] where 0=white and 1=black. *)
type pixel = float ;;
  
(* Size is number of rows (y height) by number of columns (x width);
   throughout, these widths are referred to as `row_count` and
   `col_count`, respectively *)
type size = int * int ;;
  
(* Each `image` stores the size along with the contents, as a list of
   rows (from top to bottom), each a list of column pixels (from left
   to right) 
 *)
type image = { size : size;
               content : float array array } ;;
               
(*....................................................................
  Utilities
 *)

(* rgb_of_gray value -- Returns an rgb color as for the `Graphics`
   module corresponding to a gray value of `value`, where `1.0` is
   black and `0.0` is white. Raises `Failure` if pixel value is out of
   range.
 *)
let rgb_of_gray (value : pixel) : G.color =
  if value > 1. || value < 0. then
    failwith "rgb_of_gray: value outside of range"
  else
    let level = int_of_float (float_of_int cMAXRGB *. (1. -. value)) in
    G.rgb level level level ;;

(* init_matrix row_count col_count -- Returns a fresh matrix with
   value at (row x, col y) given by `f x y`.
 *)
let init_matrix (row_count : int) (col_count : int) (f : int -> int -> 'a)
    : 'a array array =
  Array.init row_count (fun x -> Array.init col_count (fun y -> f x y))

(*....................................................................
  Basic image functions -- creation, depiction, filtering
 *)
               
(* create contents -- Creates an `image` whose values are in the range
   [0..1], with size given by `col_count` and `row_count`, and content
   as provided in `contents`.  *)
let create (content : float list list) : image =
  let row_count = List.length content in
  let col_count = List.length (List.hd content) in
  let created = Array.make_matrix row_count col_count 0. in
  content
  |> List.iteri (fun row_index row ->
                 row
                 |> List.iteri (fun col_index pixel ->
                                created.(row_index).(col_index) <- pixel));
  {size = row_count, col_count; content = created}
  
(* depict img -- Presents `img` in an OCaml graphics window and waits
   for a short period to exit the window.
 *)
let depict ({size = row_count, col_count; content} : image) : unit =
  try
    (* prepare the graphics canvas *)
    G.open_graph "";
    G.clear_graph ();
    G.resize_window col_count row_count;
    G.auto_synchronize false; (* disable automatic screen updates *)
    
    (* draw each pixel *)
    for row = 0 to row_count - 1 do
      for col = 0 to col_count - 1 do
        G.set_color (rgb_of_gray content.(row).(col));
        G.plot col (row_count - row - 1)
      done
    done;
    G.synchronize ();        (* update screen *)
    G.auto_synchronize true; (* reenable automatic screen updates *)
    
    (* pause for a couple of seconds *)
    Unix.sleep 2

  with
    (* make sure to close window if things go wrong *)
    exn -> (G.close_graph (); raise exn) ;;

(* filter f img -- Returns a fresh image where each pixel is the
   application of `f` to the corresponding pixel in `img`.  *)
let filter (f : pixel -> pixel)
           ({size = row_count, col_count; content} as img : image)
         : image =
  let new_content = init_matrix
                      row_count col_count
                      (fun row col -> f content.(row).(col)) in
  {img with content = new_content} ;;

(*....................................................................
  Various image transformations -- inversion, digital halftoning
 *)

(* invert img -- Returns an image where the light to dark values in
   `img` have been inverted.  *)
let invert : image -> image =
  filter (fun p -> (1. -. p)) ;;

(* threshold img -- Digitally halftones `img` into a binary image by
   thresholding each pixel at the given `threshold`, interpreted as a
   fraction (between 0 and 1) of the value space.  *)
let threshold (threshold : float) : image -> image =
  filter (fun p -> if threshold < p then 1. else 0.) ;;
         
(* dither img -- Digitally halftones `img` into a binary image by
   making a pixel black randomly in proportion to its gray level.  *)
let dither : image -> image =
  filter (fun p -> if Random.float 1. < p then 1. else 0.) ;;

(* error_diffuse img -- Digitally halftones `img` into a binary image
   by one-dimensional error diffusion. Relies on the fact that
   `filter` visits the pixels in a consistent row-by-row left-to-right
   order. See <https://url.cs51.io/1d-error-diffusion>. *)
let error_diffuse : image -> image =
  filter
    (let error = ref 0. in
     fun p -> if 0.5 < p +. !error then
                (error := !error -. (1. -. p);
                 1.)
              else
                (error := !error -. (0. -. p);
                 0.)) ;;
