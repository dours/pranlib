
(** 
    Binomial heap is a realisation of a priority queue - a data structure, 
	which supports fast addition of elements and finding that with a minimal priority.

   Binomial heap is a list of binomial trees.
   Binomial tree is a tree (not even binary) of size 2^k for some k. 
   It satisfies the main heap property - a priority of each node 
     is less or equal then priorities of all it's sons.
   Binomial tree of depth 0 is empty.
   Binomial tree of depth k+1 consists of a root node, which has k sons, 
     who are roots of binomial trees of depth 1, 2, ..., k.
   So, the size of bt of depth k+1 = 2^k (by induction).
   
   The representation of a heap is strongly connected with 
     the binary representation of it's size:
     a tree of size 2^k is included in the heap if and only if 
     the k-th bit of the size of the heap is set.
   
   The main operation for binomial heap is merge.
   It merges to binomial heaps in one in logarithmic time.
   The procedure of merging is similar to addition of two integers in
    binary represantation, where adding two bits is merging two binomial
    trees (we can perform this in a constant time).
    
    I use a sparse representation of a heap - some trees can be empty.
*)

(** Heap element *)
module type Element =
  sig
    type t
    val compare : t -> t -> int
    
    (** this is used for debugging purposes *)
    val toString : t -> string
  end

module Make :
  functor (X : Element) ->
    sig
    
      type 
        tree = Empty | Root of X.t * heap
      and 
        heap = tree list
      
      (** merge (h1, h2, t) merges h1, h2 and a heap consisting only of a tree t with no more than one element. 
      Thus merge is an analogue of addition of binary numbers with respect to the carry flat 
      (like adc insturction in x86 processor)*)
      val merge : heap * heap * tree -> heap
      
      (** useful internal function *)
      val join : tree -> tree -> tree -> tree * tree
      
      val fold : ('a -> X.t -> 'a) -> 'a -> heap -> 'a
      
      (** Returns a minimal element of a heap *)
      val findMin : heap -> X.t
      
      (** Returns a heap without its minimal element and that element too.
    	  You cannot remove any element, but only the minimal *)
      val removeMin : heap -> heap * X.t
      
      val add : heap -> X.t -> heap
      (** Add an element to a heap *)
            
      (** The empty heap *)
      val empty : 'a list
      
      (** Check if the heap is empty *)
      val isEmpty : tree list -> bool
      
      val iter : (X.t -> 'a) -> heap -> unit
      
      val toString : heap -> string
    end
