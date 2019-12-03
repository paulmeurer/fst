(in-package :cl-fst)

(defcfun "fdopen" :ulong
  (fd :short)
  (mode :string))

(defcfun "fflush" :short
  (stream :ulong))

(defcstruct regex
  (lex-errors :int)
  (lex-max-errors :int))

(defcstruct command-line
  (quit :int)
  (obey-ctrl-c :int)
  (stop :int)
  (want-deps :int))

(defcstruct alphabet
  (print-pairs :int)
  (print-left :int)
  (read-left :int)
  (unicode :int)
  (recode-cp1252 :int))

(defcstruct general
  (sort-arcs :int)
  (verbose :int)
  (completion :int)
  (stack :int)
  (name-nets :int)
  (minimal :int)
  (quit-on-fail :int)
  (assert :int)
  (show-escape :int)
  (sq-final-arcs :int)
  (sq-intern-arcs :int)
  (recursive-define :int)
  (recursive-apply :int)
  (compose-flag-as-special :int)
  (need-separators :int)
  (max-context-length :int)
  (vectorize-n :int)
  (fail-safe-composition :int))

(defcstruct optimization
  (in-order :int))

(defcstruct io
  (print-sigma :int)
  (print-space :int)
  (obey-flags :int)
  (mark-version :int)
  (retokenize :int)
  (show-flags :int)
  (max-state-visits :int)
  (max-recursion :int)
  (count-patterns :int)
  (delete-patterns :int)
  (extract-patterns :int)
  (locate-patterns :int)
  (mark-patterns :int)
  (license-type :int)
  (char-encoding :int)
  (use-memory-map :int)
  (use-timer :int))

(defcstruct parameters
  (interactive :int))

(defcstruct sequentialization
  (final-strings-arcs :int)
  (intern-strings-arcs :int)
  (string-one :int))

(defcstruct interface-parameters
  (regex regex)
  (command-line command-line)
  (alphabet alphabet)
  (general general)
  (optimization optimization)
  (io io)
  (parameters parameters)
  (sequentialization sequentialization))

(defcstruct page
  (line-pos :int)
  (cur-pos :int)
  (line-no :int)
  (indent :int)
  (rm :int)
  (size :int)
  (string :string)
  (eol-string :string)
  (indent-char :char))

;;; CFSM_CONTEXT

;; correct?
(defctype size_t :ulong)

(defcstruct error-stream
  #+linux ;; /* Darwin and Solaris don't support open_memstream() */
  (buffer-size size_t)
  #+linux
  (buffer :string)
  #+linux
  (memstream :pointer) ;; FILE
  #-linux
  (dummy size_t)   ; /* To keep size of struct same in both systems */
  #-linux
  (tempfile :pointer)
  #-linux
  (buffer :string))

(defcstruct label-stats
  (max-label :uint)
  (tally-size :int)
  (tally :pointer))

(defcstruct temporary-buffers
  (string-buffer :pointer)
  (fat-str-buffer :pointer)
  (page-buffer :pointer)
  (lab-vector :pointer))

(defcstruct cfsm-context
  (mode :int)
  (reclaimable :int)
  (copyright-string :string)	      ;          /* COPYRIGHT_OWNER */
  (compose-strategy :int)
  (execution-error :int)
  (in-character-encoding :int) ;  /* CHAR_ENC_UTF_8 or CHAR_ENC_ISO_8859_1 */
  (out-character-encoding :int)	; /* CHAR_ENC_UTF_8 or CHAR_ENC_ISO_8859_1 */

  (error-stream error-stream)

  (interface :pointer)

  (label-stats label-stats)
    
  (temp-bufs temporary-buffers)
  ;; more still missing
  )

;;;  /******************
;;;   * STRING BUFFER
;;;   ******************/

(defcstruct string-buffer
  (char_size :int)
  (length :int)
  (pos :int)
  (lines :int)
  (string :string))

;;;  /*****************
;;;   * APPLY CONTEXT
;;;   *****************/

#+not-yet
(defcstruct apply-context
  (reclaimable :int)			;
  (net1 :pointer)			 ;             /* Net to be applied */
  (NETptr :pointer)	;             /* Second network for bimachines -- not used now */
  (net-vector :pointer)			;
  (side :int)	     ;                /* Input side: LOWER or UPPER */
  (out_side :int)	;            /* Output side: UPPER or LOWER */
  (obey_flags_p :int) ;        /* 1 = obey flag diacritics, 0 = don't obey */
  (print_space_p :int) ;       /* Separate output symbols by a space */
  (show_flags_p :int) ;        /* Show flag diacritics in the output */
  (flags_p :int)	   ;             /* 1 = Network has flag diacritics */
  (recursive_p :int)			;
  (eol_is_eof_p :int)			;
  (next_input_line_p :int)		;
  (need_separators_p :int) ;   /* separators required in apply_patterns().  */
  (count_patterns_p :int) ;    /* count pattern matches                     */
  (delete_patterns_p :int) ;   /* delete material that matches              */
  (extract_patterns_p :int) ;  /* delete material that does not match       */
  (locate_patterns_p :int) ;   /* locate begin end and tag                  */
  (one_tag_per_line_p :int) ;  /* print pattern locations on seperate lines */
  (mark_patterns_p :int)	;     /* mark patterns with tags                   */
  (max_context_length :int) ;  /* maximal length of left and right context  */
  (in_pos :int)			   ;              /* input position */
  (end_pos :int)		      ;             /* end of pattern match */
  (nv_pos :int)				;
  (level :int)				;
  (depth :int)				;
  (num_inputs :int)	   ;          /* number of processed inputs */
  (eol_string :string)		;  /* defaults to "\n" */
  (end_of_input :int)	    ;        /* end of input file or string */
  (longest_match :int)	     ;       /* longest pattern match found */
  (max_recursion_depth :int) ; /* apply_patterns is implemented as a recursion */
  ;; /* The role of this variable is to bound this   */
  ;;/* recursion to avoid stack overflow (mainly    */
  ;;/* for very general patterns that can go */
  ;;/* arbitrarily long). */
  ;;/* If set to -1 it won't be bounded */
  (parse_table :pointer)   ;   /* Maps input symbol to a symbol ID */
  (next_symbol_fn :pointer) ; /* fetches the next input ID */
  (write_buffer_fn :pointer) ;  /* Function to write into out_buffer */
  (in_fn :pointer)  ;        /* lower_id() or upper_id() */
  (out_fn :pointer)	;       /* upper_id() or lower_id() */
  (prev_sym :uint)			;
  (match_table :poineter)		;
  (input :string)      ;             /* current input string */
  (remainder :string) ;         /* remaining part of the input string */
  (in_stream :ulong)		 ;                  /* input stream */
  (out_stream :ulong)		 ;                 /* output stream */
  (in_data :void)			;
  (out_data :void)			;
  (out_count :int)	     ;                    /* output counter */
  (output_fn :pointer)	;   /* output function */
  (in_vector :pointer) ;          /* vector for storing input IDs */
  (mid_vector :pointer)		;
  (out_vector :pointer) ;         /* vector for storing output IDs */
  (in_table :pointer)	;
  (out_table :pointer)	;
  (sigma :pointer)			;
  (prev_sigma :pointer)		;
  (host_net_vector :pointer)		;
  (flag_register :pointer)		;
  (flag_vector :pointer)		;
  (tag_vector :pointer)		;
  (arc_vector :pointer)		;
  (state_vector :pointer)		;
  (destination_vector :pointer)	;
  (start_vector :pointer)		;
  (task_vector :pointer)		;
  (pos_table :ointer)		;
  (in_buffer :pointer)		;
  (out_buffer :pointer)		;
  (save_buffer :pointer)	;
  (void *hyper_unit)			;
  (uintptr_t file_pos)			;

  (LAB_VECTORptr other_than_vector)	;

  (IO_SEQptr in_seq)			;
  (IO_SEQptr out_seq)			;
  (IO_SEQ_TABLEptr input_table)		;
  (IO_SEQ_TABLEptr output_table)	;

  /* Net traversal call-back functions: */
  (start_state_fn :pointer)
  (label_from_arc_fn :pointer)
  (next_arc_fn :pointer)
  (destination_fn :pointer)

  (solution_tree :pointer)
  ;;  /* For storing the final result in a tree
  ;; instead of the table. */
  (input_ring :pointer)		;
  (location_path :pointer) ; /* Application path in a network. */
  (location_path_length :int)		;  /* Size of location path */
  (task_heap :pointer) ;         /* Heap for iterative_apply_patterns() */
  (task_stack :pointer)	;       /* Stack for iterative apply_patterns() */
  (state :pointer)			;
  (arc_it :pointer))



#||
  typedef struct CFSM_CONTEXT {
    int mode ;
    int reclaimable;
    char *copyright_string ;          /* COPYRIGHT_OWNER */
    int compose_strategy ;
    int execution_error;
    int in_character_encoding ;  /* CHAR_ENC_UTF_8 or CHAR_ENC_ISO_8859_1 */
    int out_character_encoding ; /* CHAR_ENC_UTF_8 or CHAR_ENC_ISO_8859_1 */

    ERROR_STREAM errorstream;

    IntParPtr interface;

    struct LABEL_STATS label_stats;

    struct temporary_buffers {
      STRING_BUFFERptr string_buffer;
      STRING_BUFFERptr fat_str_buffer;
      PAGEptr page_buffer;
      LAB_VECTORptr lab_vector;
    } temp_bufs ;
    struct flag_parameters
    {
      int keep_p ;
      int determinize_p ;
      int minimize_p ;
      int prune_p ;
      int reclaim_p ;
      int embedded_command_p;
    } flags ;
    struct pretty_print_parameters
    {
      int cur_pos ;                    /* cur_pos */
      int indent ;
      int line_pos ;
      char *output_buffer ;            /* output_buffer */
      int output_buffer_size ;         /* OUTPUT_BUFFER_SIZE */
      int right_margin ;
      char *eol_string ;
    } pretty_print ;
    struct path_index_data
    {
      int max_path_index_pos ;         /* MAX_PATH_INDEX_POS */
      int path_index_incr ;            /* PATH_INDEX_INCR */
      int path_index_pos ;             /* PATH_INDEX_POS */
      intptr_t *path_index_vector ;    /* PATH_INDEX_VECTOR */
    } index ;
    struct parse_parameters_and_data
    {
      int ignore_white_space_p ;
      int zero_to_epsilon_p ;
      int input_seq_size ;              /* WORD_STRING_SIZE */
      id_type *input_seq ;              /* INPUT_SEQ */
      id_type *lower_match ;            /* LOWER_MATCH */
      id_type *match_table ;            /* MATCH_TABLE */
      id_type *upper_match ;            /* UPPER_MATCH */
      int obsolete_parse_tables ;       /* PTBL_OBSOLETE */
    } parse;
    struct bin_io_parameters_and_data
    {
      int altchain_p ;                /* ALTCHAIN_P */
      int status_bar_p ;              /* DISPLAY_READ_STATUS_BAR */
      int32 status_bar_increment ;    /* STATUS_BAR_INCREMENT */
      uint32 arc_count ;              /* ARC_COUNT */
      byte cur_byte ;                 /* CUR_BYTE */
      uintptr_t byte_count ;	      /* BYTE_COUNT */
      STANDARD_HEADERptr last_header; /* LAST_HEADER */
      STANDARD_HEADERptr next_header; /* NEXT_HEADER */
      STATEptr  cur_state ;           /* CUR_STATE */
      STATEptr  state_stack ;         /* STATE_STACK */
      STATEptr *state_vector ;	      /* STATE_VECTOR */
      char **attributes ;             /* STANDARD_ATTRIBUTES */
      int attribute_count ;           /* STANDARD_ATTRIBUTE_COUNT */
      int memory_map ;                /* MEMORY_MAP */
      char *current_filename;         /* CURRENT_FILENAME */
    } bin_io ;
    struct define_data
    {
      HASH_TABLEptr net_table;        /* DEF_TABLE */
      HASH_TABLEptr set_table;
    } define;
  } FST_CNTXT, *FST_CNTXTptr;
||#


#||

  /*****************
   * APPLY CONTEXT
   *****************/

  typedef struct APPLY_CONTEXT {
    int reclaimable;
    NETptr net1;             /* Net to be applied */
    NETptr net2;             /* Second network for bimachines -- not used now */
    NVptr net_vector;
    int side;                /* Input side: LOWER or UPPER */
    int out_side;            /* Output side: UPPER or LOWER */
    int obey_flags_p;        /* 1 = obey flag diacritics, 0 = don't obey */
    int print_space_p;       /* Separate output symbols by a space */
    int show_flags_p;        /* Show flag diacritics in the output */
    int flags_p;             /* 1 = Network has flag diacritics */
    int recursive_p;
    int eol_is_eof_p;
    int next_input_line_p;
    int need_separators_p;   /* separators required in apply_patterns().  */
    int count_patterns_p;    /* count pattern matches                     */
    int delete_patterns_p;   /* delete material that matches              */
    int extract_patterns_p;  /* delete material that does not match       */
    int locate_patterns_p;   /* locate begin end and tag                  */
    int one_tag_per_line_p;  /* print pattern locations on seperate lines */
    int mark_patterns_p;     /* mark patterns with tags                   */
    int max_context_length;  /* maximal length of left and right context  */
    int in_pos;              /* input position */
    int end_pos;             /* end of pattern match */
    int nv_pos;
    int level;
    int depth;
    int num_inputs;          /* number of processed inputs */
    const char *eol_string;  /* defaults to "\n" */
    int end_of_input;        /* end of input file or string */
    int longest_match;       /* longest pattern match found */
    int max_recursion_depth; /* apply_patterns is implemented as a recursion */
                             /* The role of this variable is to bound this   */
                             /* recursion to avoid stack overflow (mainly    */
                             /* for very general patterns that can go */
                             /* arbitrarily long). */
                             /* If set to -1 it won't be bounded */
    PARSE_TBL parse_table;   /* Maps input symbol to a symbol ID */
    int (*next_symbol_fn)(id_type *, void *); /* fetches the next input ID */
    void (*write_buffer_fn)(void *);  /* Function to write into out_buffer */
    id_type (*in_fn)(id_type);        /* lower_id() or upper_id() */
    id_type (*out_fn)(id_type);       /* upper_id() or lower_id() */
    id_type prev_sym;
    MATCH_TABLEptr match_table;
    unsigned char *input;             /* current input string */
    unsigned char *remainder;         /* remaining part of the input string */
    FILE *in_stream;                  /* input stream */
    FILE *out_stream;                 /* output stream */
    void *in_data;
    void *out_data;
    int out_count;                    /* output counter */
    void (*output_fn)(void *cntxt);   /* output function */
    LAB_VECTORptr in_vector;          /* vector for storing input IDs */
    LAB_VECTORptr mid_vector;
    LAB_VECTORptr out_vector;         /* vector for storing output IDs */
    LAB_VECTOR_TABLEptr in_table;
    LAB_VECTOR_TABLEptr out_table;
    ALPHABETptr sigma;
    ALPHABETptr prev_sigma;
    VECTORptr host_net_vector;
    ALPHABETptr flag_register;
    LAB_VECTORptr flag_vector;
    LAB_VECTORptr tag_vector;
    VECTORptr arc_vector;
    VECTORptr state_vector;
    VECTORptr destination_vector;
    VECTORptr start_vector;
    VECTORptr task_vector;
    VECTOR_TABLEptr pos_table;
    IN_BUFFERptr in_buffer;
    STRING_BUFFERptr out_buffer;
    STRING_BUFFERptr save_buffer;
    void *hyper_unit;
    uintptr_t file_pos;

    LAB_VECTORptr other_than_vector;

    IO_SEQptr in_seq;
    IO_SEQptr out_seq;
    IO_SEQ_TABLEptr input_table;
    IO_SEQ_TABLEptr output_table;

    /* Net traversal call-back functions: */
    void* (*start_state_fn)(NETptr, void**, int*);
    id_type (*label_from_arc_fn)(NETptr, void**, int*, int*);
    void (*next_arc_fn)(NETptr, void**, int);
    void* (*destination_fn)(NETptr, void**);

    STATEptr solution_tree;  /* For storing the final result in a tree
                                instead of the table. */
    LAB_RINGptr input_ring;
    LOCATION_PATHptr location_path; /* Application path in a network. */
    int location_path_length;  /* Size of location path */
    HEAPptr task_heap;         /* Heap for iterative_apply_patterns() */
    STACKptr task_stack;       /* Stack for iterative apply_patterns() */
    STATEptr state;
    ARCITptr arc_it;
  } APPLYtype, *APPLYptr;
||#



#||
  /*********************
   *  NETWORK
   *********************/

  NETptr make_empty_net(void);
  /* Returns a skeleton network structure with an empty sigma and
     label_alphabets but without an initial state. Use either
     null_net() or epsilon_net() to create a minimal network
     with an initial state. */

  NETptr copy_net(NETptr net);
  /* Returns a copy of the network. */

  int minimize_net(NETptr net);
  /* Destructively minimizes the network using Hopcroft's algorithm.
     Returns 0 on success and 1 on error. As a prelimnary step
     to minimization, the network is first pruned, epsilons are
     removed and the network is determinized. Minimization can
     only be done on standard networks, not on networks that have
     been compacted or vectorized. */

  void free_network(NETptr net);
  /* Returns the network to the global network heap. */
||#


(defcfun "free_network" :void
  (net :pointer))

#||
  void print_net(NETptr net, FILE *stream);
  /* Prints the states and arcs of the network into the stream. */

  STATEptr add_state_to_net(NETptr net, int final_p);
  /* Adds a new state to the network. If final_p is non-zero, the
     state is final. Returns the new state on success, NULL on failure. */

  ARCptr add_arc_to_state(NETptr net, STATEptr start, id_type id, STATEptr dest,
                          void *user_pointer, int big_arc_p);
  /* Creates a new arc from start to dest with the label id unless it
     would duplicate an existing arc. Does not add a looping EPSILON
     arc.  The start and dest states must already exist in the
     network. The network must be a standard network, not vectorized
     or optimized. Updates the sigma and label alphabets of the
     network. If big_arc_p is non-zero, the new arc will have a
     user_pointer field. Returns the arc on success, NULL on failure. */

  int read_net_properties(NETptr net, char *file);
  /* Reads a list of attribute value pairs from the file and adds them
     to the networks property list. For example,
     NETWORKNAME: "Number-to-numeral converter"
     LARGEST_NUMBER: 99999
     If file is NULL, the input is obtained from stdin. */

  int write_net_properties(NETptr net, char *file);
  /* Writes the networks property list into a file or to stdout if
     file is NULL. */

  int add_string_property(NETptr net, char *attribute, char *value);
  /* Adds the attribute:value pair to the network's property list.
     Any previous value for the attribute is freed and replaced by
     the new value. Returns 0 on success, 1 on error.  Both the
     attribute and the value are copied and converted to fat
     strings, they can be freed by the calling function if they
     have been malloc'd. */

  char *get_string_property(NETptr net, char *attribute);
  /* Returns the value of the attribute on the property list of the
     net, or NULL if it is not found. The value is a freshly allocated
     C string. It should be freed by the calling function when it
     is not needed anymore. */

  int remove_string_property(NETptr net, char *attribute);
  /* Removes the attribute and its value from the property list of the
     network. Returns 0 on success, 1 or error. */



  /*************************
   *  LABEL ID MAP
   ***************************/

  typedef struct LABEL_ID_MAP LABEL_ID_MAPtype, *LABEL_ID_MAPptr;
  /* A data structure structure for associating label names (fat
     strings) and the corresponding integer IDs. It contains a hash
     table that maps label names to their IDs and an array of
     labels. The label for an ID, for example 321, is located at the
     position 321 in the label array. The maximum number of label IDs
     used to be 65535, it is now 16777214 (2^24 -2).  When the default
     label map is initialized, all the printable ASCII characters get
     a label and an ID that is the same as the integer value of the
     character. For example, the symbol 'A' has the ID 65. Labels for
     other symbols and symbol pairs are created on demand.  Special
     symbols that have fixed label IDs include EPSILON (ID 0) and
     OTHER (ID 1), the unknown symbol. Because symbol names are
     recorded as fat strings, they do not depend on the character
     encoding mode (utf-8 or iso-8859-1). */
||#

(defcfun "single_to_id" :uint
  (name :string))

(defcfun "pair_to_id" :uint
  (upper :string)
  (lower :string))

(defcfun "id_pair_to_id" :uint
  (upper-id :uint)
  (lower-id :uint))

#||
  id_type id_pair_to_id(id_type upper_id, id_type lower_id);
  /* Returns the ID of the tuple label upper_id and lower_id as the
     two components.  The names are processed as either UTF8 strings or
     Latin-1 strings depending on the mode. If upper_id and lower id are
     identical, the result is identical to them as well. */

  LABELptr id_to_label(id_type id);
  /* Returns the label corresponding to the id. */

  id_type upper_id(id_type id);
  /* Returns the upper id of a tuple label or the id itself if id
     refers to an atomic label. */

  id_type lower_id(id_type id);
  /* Returns the lower id of a tuple label or the id itself if id
     is an atomic label. */

||#

(defcfun "libcfsm_version" :string)

;;  /* Initialization and reclamation of CFSM context */

(defcfun "initialize_cfsm" :pointer)

(defcfun "reclaim_cfsm" :pointer
  (cntxt-ptr :pointer))

(defcfun "get_default_cfsm_context" :pointer)

(defcfun "int_parameters" interface-parameters)

(defcfun "save_net" :int
  (net :pointer)
  (filename :string)
  (fst-cntxt :pointer))

(defcfun "save_nets" :int
  (nv :pointer)
  (filename :string)
  (fst-cntxt :pointer))

(defcfun "save_defined_nets" :int
  (filename :string)
  (fst-cntxt :pointer))

;;  /* Binary input functions */

(defcfun "load_net" :pointer
  (filename :string)
  (fst-cntxt :pointer))

(defcfun ("load_net" load-net1) :pointer
  (filename :pointer)
  (fst-cntxt :pointer))

(defcfun "load_nets" :pointer
  (filename :string)
  (fst-cntxt :pointer))

(defcfun "load_defined_nets" :int
  (filename :string)
  (fst-cntxt :pointer))

(defcfun "load_defined_net" :pointer
  (name :string)
  (filename :string)
  (fst-cntxt :pointer))

;;  /* Text input functions */

(defcfun "string_to_net" :pointer
  (str :string)
  (byte-pos-p :int))

(defcfun "read_text" :pointer
  (filename :string))

(defcfun "read_spaced_text" :pointer
  (filename :string))

(defcfun "read_regex" :pointer
  (regex-str :string))

(defcfun "read_lexc" :pointer
  (filename :string))

(defcfun "read_prolog" :pointer
  (filename :string))

;;;  /* Text output functions */

(defcfun "write_text" :int
  (net :pointer)
  (filename :string))

(defcfun "write_text_to_page" :int
  (net :pointer)
  (page :pointer))

(defcfun "write_spaced_text_to_page" :int
  (net :pointer)
  (page :pointer))

(defcfun "write_prolog" :int
  (net :pointer)
  (filename :string))

;;  /* Optimizations */

(defcfun "vectorize_states" :int
  (net :pointer)
  (min-num-arcs :int))

(defcfun "unvectorize_net" :int
  (net :pointer))

(defcfun "optimize_arcs" :void
  (net :pointer))

(defcfun "unoptimize_arcs" :void
  (net :pointer))

(defcfun "share_arcs" :int
  (net :pointer))

(defcfun "unshare_arcs" :int
  (net :pointer)
  (keep-p :int))

(defcfun "reduce_labelset" :void
  (net :pointer))

(defcfun "unreduce_labelset" :void
  (net :pointer))

(defcfun "compact_net" :void
  (net :pointer))

(defcfun "uncompact_net" :void
  (net :pointer))

;;  /* Applying transducers to strings and streams */

(defcfun "init_apply" :pointer
  (net :pointer)
  (side :int)
  (cfsm-cntxt :pointer))

(defcfun "apply_to_string" :string
  (input :string)
  (applyer :pointer))

(defcfun "switch_input_side" :void
  (applyer :pointer))

(defcfun "new_applyer" :pointer
  (net :pointer)
  (string :pointer)
  (stream :ulong)
  (input-side :int)
  (cfsm-cntxt :pointer))

(defcfun "make_applyer" :pointer
  (file :string)
  (in-string :pointer)
  (in-file :string)
  (input-side :int)
  (cfsm-cntxt :pointer))

(defcfun "next_apply_output" :pointer
  (applyer :pointer))

(defcfun "new_pattern_applyer" :pointer
  (net :pointer)
  (in-string :pointer)
  (in-stream :ulong)
  (out-stream :ulong)
  (input-side :int)
  (cfsm-cntxt :pointer))

(defcfun "make_pattern_applyer" :pointer
  (fst-file :string)
  (in-string :pointer)
  (in-file :string)
  (out-file :string)
  (input-side :int)
  (cfsm-cntxt :pointer))

(defcfun "next_pattern_output" :pointer
  (applyer :pointer))

(defcfun "apply_patterns" :int
  (applyer :pointer))

(defcfun "init_apply_to_string" :void
  (input :string)
  (apply-context :pointer))

(defcfun "init_apply_to_stream" :void
  (file :pointer)
  (apply-context :pointer))

(defcfun "pattern_match_counts_to_page" :pointer
  (pattern-applyer :pointer)
  (page :pointer))

(defcfun "free_applyer" :void
  (applyer :pointer))

(defcfun "free_applyer_complete" :void
  (applyer :pointer))

#||
  /* Unary Tests */

  int test_lower_bounded(NETptr net);
  /* Returns 1 if the lower  side of the network has no epsilon loops,
     otherwise 0. */

  int test_upper_bounded(NETptr net);
  /* Returns 1 if the upper  side of the network has no epsilon loops,
     otherwise 0. */

  int test_non_null(NETptr net);
  /* Returns 1 if the network is not a null fsm, that is, a network that
     has no reachable final state, otherwise 0. */

  int test_upper_universal(NETptr net);
  /* Returns 1 if the upper side of the network is the universal (sigma-star)
     langugage that contains any string of any length, including the empty
     string, otherwise 0 */

  int test_lower_universal(NETptr net);
  /* Returns 1 if the upper side of the network is the universal (sigma-star)
     langugage that contains any string of any length, including the empty
     string, otherwise 0 */

  /* Binary network tests */

  int test_equivalent(NETptr net1, NETptr net2);
  /* Returns 1 if net1 and net2 are structurally equivalent, otherwise 0.
     Two networks are structurally equivalent just in case they, have the
     same arity, the same sigma and label alphabet, the same number of arcs
     and states and equivivalent paths. If the arity is 1 and the
     networks are structurally equivalent, they encode the same language.
     If net1 and net2 are structurally equivalent transducers, they encode
     the same relation. If two transducers are not structurally equivalent,
     they may nevertheless encode the same relation by having epsilons in
     different places. The equivalence of transducers is no decidable in
     the general case. */

  int test_sublanguage(NETptr net1, NETptr net2);
  /* Returns 1 if the language or relation of net1 is a subset of the
     language of relation of net2. The test is correct when net1 and
     net2 encode simple languages but it is not generally correct for
     transducers for the reason explained above. */

  int test_intersect(NETptr net1, NETptr net2);
  /* Returns 1 if the languages or relations encoded by net1 and net2
     have strings or pairs of strings in common. The test is correct
     for simple networks but not generally correct for transducers
     for the reason explained above. */

  /* Definitions */

  int define_net(char *name, NETptr net, int keep_p);
  /* Binds the name to network. If the keep_p flag is KEEP, the
     name is bound to the copy of the network. The name can be
     used in a regular expression to splice in a copy of the
     defined network. Returns 0 on success, 1 on error. */

  int define_regex_net(char *name, char *regex);
  /* Compiles the regular expression and binds the name to it
     by calling define_net(). Returns 0 on success, 1 on error. */

  int undefine_net(char *name);
  /* Frees the network the name is bound to and unbinds the name.
     Returns 0 on success, 1 on error. */

  NETptr get_net(char *name, int keep_p);
  /* Returns the network the name is bound to, or its copy if
     keep_p is KEEP. Returns NULL if the name is undefined. */

  NETptr net(char *name);
  /* Returns get_net(name, DONT_KEEP). */

  int define_regex_list(char *name, char *regex);
  /* Compiles the regular expression and binds the name to the sigma
     alphabet of the resulting network by calling
     define_symbol_list(). The rest of the network structure is
     reclaimed. Returns 0 on success, 1 on error.
     A list name can be used in a regular expression to
     refer to the union of the symbols it contains. For example,
     define_regex_list("Vowel", "a e i o u") binds Vowel to the
     alphabet containing the five vowels. Given this definition,
     read_regex("Vowel") is equivalent to read_regex("a|e|i|o|u").
     Names that are bound to a list may also be used in so-called
     "list flags", special symbols of the form @L.name@ and @X.name@
     where L means 'member of the list' and X means 'excluding members
     of the list'. The apply operations recognize an arc labeled
     "@L.Vowel@" as standing for any member of the list Vowel. In
     contrast, calculus operations do not currently assign any special
     interpretation to list flags. */

  int define_symbol_list(char *name, ALPHABETptr alph, int keep_p);
  /* Binds the name to the alphabet, or to its copy if keep_p is
     KEEP. Returns 0 on success, 1 on error. */

  ALPHABETptr get_symbol_list(char *name, int keep_p);
  /* Returns the alphabet the name is bound to, or its copy if
     keep_p is KEEP. Returns NULL if the name is not
     bound to an alphabet. */

  ALPHABETptr symbol_list(char *name);
  /* Returns get_symbol_list(name, DONT_KEEP). */

  int undefine_symbol_list(char *name);
  /* Unbinds the name and reclaims the alphabet it was bound to.
     Returns 0 on success and 1 on error. */

  int define_function(char *fn_call, char *regex);
  /* Compiles the regular expression and binds it to the function
     call. For example, define_function("Double(X)," "X X");
     creates a simple function that concatenates the argument
     to itself. Functions are used in regular expression. Given
     the definition of "Double(X)", read_regex("Double(a)");
     is equivalent to read_regex("a a"). See the piglatin
     application for examples of more interesting function
     definitions. */

  /* Primitive network constructors */

  NETptr null_net(void);
  /* Returns a network consisting of a single non-final state.  It
     encodes the null language, a language that contains nothing, not
     even the empty string. Equivalent to read_regex("\?"); */

  NETptr epsilon_net(void);
  /* Returns a network consisting of a single final state.
     It encodes the language consisting of the empty string.
     Equivalent to read_regex("[]"); */

  NETptr kleene_star_net(void);
  /* Returns a network consisting of a single final state
     with a looping arc for the unknown symbol. It encodes
     the universal ("sigma star") language.
     Equivalent to read_regex("?*"); */

  NETptr kleene_plus_net(void);
  /*  Returns a network that encodes the universal language
      minus the empty string. Equivalent to read_regex("?+"); */

  NETptr label_net(id_type id);
  /* Returns a network that encodes the string or a pair of strings
     represented by the id. */

  NETptr symbol_net(char *sym);
  /* Returns label_net(single_to_id(sym)); */

  NETptr pair_net(char *upper, char *lower);
  /* Returns label_net(pair_to_id(upper, lower)); */

  NETptr alphabet_net(ALPHABETptr alph);
  /* Returns the network that encodes the language of the union of the
     singleton languages or relations represented by the labels in the
     alphabet. */

  /* Sigma and  Label alphabets */

  /* The return values of net_sigma() and net_labels() are the actual
     alphabets of the network. They must not be modified directly, and
     they will not be up-to-date if the network is modified. */

  ALPHABETptr net_sigma(NETptr net);
  /* Returns the network's sigma alphabet. */

  ALPHABETptr net_labels(NETptr net);
  /* Returns the network's label alphabet. */

  void update_net_labels_and_sigma(NETptr net);
  /* Updates the label and the sigma alphabet of the network and
     the network arity (1 or 2). After the update, the label
     label alphabet contains all and only labels that appear
     on some arc of the network. Any missing symbols are
     added to the sigma alphabet. */

  /* Shifting initial epsilons*/
  NETptr shift_initial_epsilons(NETptr /*net*/, unsigned /*max_num_of_epsilons*/);

  /* Substitutions */

  /* If the keep_p argument is KEEP, the argument networks are
     preserved unchanged. If the keep_p argument is DONT_KEEP,
     the argument networks are reclaimed or destructively modified.
     The alphabet arguments are preserved unchanged. */

  NETptr substitute_symbol(id_type id, ALPHABETptr list, NETptr net,
                           int keep_p);
  /* Replaces every arc that has id in its label by a set of arcs
     labeled by symbols created by replacing id by a member of the
     list. All the new arcs have the same destination as the original
     arc. If id is itself a member of the list, the original arc is
     reconstituted in the process. If the list is NULL or has no
     members, then all arcs that have id in their label are
     eliminated. The label and sigma alphabets are updated.  If keep_p
     is KEEP, the operation is performed on a copy of the original
     network. Returns the modified network. */

  NETptr substitute_label(id_type id, ALPHABETptr labels, NETptr net,
                          int keep_p);
  /* Like substitute_symbol() except that id is treated as a label an
     not as a label component. For example, if id represents "a", then
     only arcs with "a" as the label are affected but arcs such as
     "a:b" do not get changed. If keep_p is KEEP, the operation is
     performed on a copy of the original network. Returns the modified
     network. */

  NETptr substitute_net(id_type id, NETptr insert, NETptr target,
                        int keep_insert_p, int keep_target_p);
  /* Replaces the arcs labeled with id in the target by splicing
     a keep of the insert network between the start state of
     the arc and its destination. If keep_insert_p is KEEP, the
     the insert network is not affected by the operation.
     If keep_p is DONT_KEEP, the insert network is reclaimed.
     The target network is destructively modified if keep_target_p
     is DONT_KEEP. If keep_target_p is KEEP, the operation is
     performed on a copy of the target network. Returns the
     resulting network. */

  NETptr close_net_alphabet(NETptr net);
  /*  Closes the alphabet of the network by removing all arcs
      with OTHER in their label. Returns the modified network. */

  NETptr eliminate_flag(NETptr net, char *name, int keep_p);
  /* Eliminates all arcs that have name as an attribute of a flag
     diacritic such as @U.Case.Acc@ or as a list symbol in a list flag
     such as @L.Vowel@ or as a defined network in an insert flag such
     as @I.FirstName@. In the case of a flag diacritic such as
     @U.Case.Acc@, the function constructs a constraint network and
     composes it with net (or a copy of it) to enforce the constraint.
     In the case of a list or an insert flag, the function eliminates
     the arcs in question by splicing in a network. Returns the
     modified network or the copy of it if keep_p is KEEP. */

  /* Alphabet operations */

  /* If the keep_p argument is KEEP, the argument alphabets are
     preserved unchanged. If the keep_p argument is DONT_KEEP, the
     argument alphabets are reclaimed or destructively modified. If
     there is no keep_p flag, the operation is non-destructive. The
     alphabets may be of either of the two types, binary vectors or
     label alphabets. */

  ALPHABETptr alph_add_to(ALPHABETptr alph, id_type new_id,
                          int keep_p);
  /* Adds new_id to the alphabet. If keep_p is KEEP, the
     operation is made on a copy of alph. Returns the
     modified alphabet. */

  ALPHABETptr alph_remove_from(ALPHABETptr alph, id_type id,
                               int keep_p);
  /* Removes id from the alphabet or from its copy if keep_p is
     KEEP. Returns the modified alphabet. */

  ALPHABETptr union_alph(ALPHABETptr alph1, ALPHABETptr alph2,
                         int keep_alph1_p, int keep_alph2_p);
  /* Returns the union of the two alphabets. If the keep_p
     arguments are DONT_KEEP the input alphabets are reclaimed. */

  ALPHABETptr intersect_alph(ALPHABETptr alph1, ALPHABETptr alph2,
                             int keep_alph1_p, int keep_alph2_p);
  /* Returns the intersection of the two alphabets. If the keep_p
     arguments are DONT_KEEP, the orignals are reclaimed. */

  ALPHABETptr minus_alph(ALPHABETptr alph1, ALPHABETptr alph2,
                         int keep_alph1_p, int keep_alph2_p);
  /* Returns a new binary alphabet containing all the IDs in alph1
     that are not in alph2. The input alphabets are reclaimed
     unless the keep_p flags are KEEP. */

  ALPHABETptr binary_to_label(ALPHABETptr alph);
  /* Converts the alphabet from binary to label format, if it is not
     in the label format already. */

  ALPHABETptr label_to_binary(ALPHABETptr alph);
  /* Converts the alphabet from label to binary format, if it is not
     in the label format already. */

  int test_equal_alphs(ALPHABETptr alph1, ALPHABETptr alph2);
  /* Returns 1 if alph1 and alph2 contain the same IDs, otherwise
     0. */

  int test_alph_member(ALPHABETptr alph, id_type id);
  /* Returns 1 if id is a member of the alphabet, otherwise 0. */

  /* Network operations */

  /* If the keep_X_p argument is KEEP, the corresponding network is
     preserved unchanged. If the keep_X_p argument is DONT_KEEP, the
     network X is reclaimed or destructively modified. Most
     network operations presuppose that the arguments are
     standard networks that have not been compacted, vectorized,
     or optimized. */

  /* Unary operations. */

  NETptr lower_side_net(NETptr net, int keep_p);
  /* Extracts the lower-side projection of the net. That is, every arc
     with a pair label is relabeled with the lower side id of the
     pair. Returns the modified network. The corresponding regular
     expression operator is .l. */

  NETptr upper_side_net(NETptr net, int keep_p);
  /* Extracts the upper-side projection of the net. That is, every arc
     with a pair label is relabeled with the upper side id of the
     pair. Returns the modified network. The corresponding regular
     expression operator is the suffix .u. */
||#


(defcfun "invert_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "reverse_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "contains_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "optional_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "zero_plus_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "one_plus_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "negate_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "other_than_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "substring_than_net" :pointer
  (net :pointer)
  (keep-p :int))

(defcfun "repeat_net" :pointer
  (net :pointer)
  (min :int)
  (max :int)
  (keep-p :int))

;;;  /* Binary network operations. */

(defcfun "shuffle_net" :pointer
  (net1 :pointer)
  (net2 :pointer)
  (keep-net1-p :int)
  (keep-net2-p :int))

(defcfun "concat_net" :pointer
  (net1 :pointer)
  (net2 :pointer)
  (keep-net1-p :int)
  (keep-net2-p :int))

#||
  NETptr concat_net(NETptr net1, NETptr net2, int keep_net1_p,
                    int keep_net2_p);
  /* Returns the concatenation of net1 and net2, that is, a network in
     which every path in net1 is continued with every path in net2. If
     keep_net1_p is DONT_KEEP, net1 will be destructively modified and
     returned as the result. If keep_net2_p is DONT_KEEP, net2 will be
     used up and reclaimed. The corresponding regular expression for the
     concatenation operator is empty space between symbols. */

  NETptr union_net(NETptr net1, NETptr net2, int keep_net1_p,
                   int keep_net2_p);
  /* Returns the union of the two networks, that is, a network
     containg all the paths of the two networks. If keep_net1_p is
     DONT_KEEP, net1 will be destructively modified and returned as
     the result. If keep_net2_p is DONT_KEEP, net2 will be used up and
     reclaimed. The corresponding regular expression operator is |. */

  NETptr intersect_net(NETptr net1, NETptr net2, int reclaim_net1_p,
                       int reclaim_net_p);
  /* Returns a new network containing the paths that are both in net1
     and net2. Intersection is not well-defined for transducers that
     contain epsilon symbols in symbol pairs such as a:0. If
     reclaim_net1_p or reclaim_net2_p is DONT_KEEP, the network will
     be reclaimed, otherwise it will remain. The corresponding
     regular expression operator is &. */

  NETptr minus_net(NETptr net1, NETptr net2, int keep_net1_p,
                   int keep_net2_p);
  /* Returns a network that contains all the paths in net1 that are
     not in net2. The minus operation is not well-defined for
     transducers that contain epsilon symbols. The minus operation can
     be used to produce a complement of a simple relation. For
     example, minus_net(read_regex("?:?"), read_regex("a:b"),
     DONT_KEEP) that maps any symbol to itself and to any other symbol
     except that the pair a:b is missing. The correspoding regular
     expression operator is -. */

  NETptr compose_net(NETptr upper, NETptr lower, int keep_upper_p,
                     int keep_lower_p);
  /* Returns the composition of the two networks. The corresponding
     regular expression operator is .o. */

  NETptr crossproduct_net(NETptr upper, NETptr lower, int keep_upper_p,
                          int keep_lower_p);
  /* Returns a network that pairs all the strings in the languages of
     the two networks with each other. If keep_p is DONT_KEEP the
     network is reclaimed. The corresponding regular expression
     operators are .x. (low binding preference) and : (high binding
     preference. */

  NETptr ignore_net(NETptr target, NETptr noise, int keep_target_p,
                    int keep_noise_p);
  /* Returns a network that is like the target except that every state
     of the network contains a loop that contains all the paths of the
     noise network. For example, ignore_net(read_regex("a b c"),
     symbol_net("x"), DONT_KEEP, DONT_KEEP); returns a language that
     contains the string "abc" and an infinite number of strings such
     as "axbcxb" that contain bursts of noise. The correspoding
     regular expression operator is /. */

  NETptr priority_union_net(NETptr net1, NETptr net2, int side,
                            int keep_net1_p, int keep_net2_p);
  /* Returns a network that represents the union of net1 and net2 that
     gives net1 preference over net2 on the given side (UPPER or
     LOWER).  For example, if the side is UPPER and net1 consists of
     the pair a:b and net2 consists of the pairs a:c and d:e, the
     priority union of the two consists of the pairs a:b and d:e. The
     a:c pair from net2 is discarded because net1 has another pair
     with a on the upper side. The d:e pair from net2 is included
     because net1 has no competing mapping for the upper side d. The
     corresponding regular expression operators are .p. for priority
     union on the LOWER side and .P. for UPPER priority union. */

  NETptr lenient_compose_net(NETptr upper, NETptr lower, int keep_upper_p,
                             int keep_lower_p);
  /* A function for experimenting with optimality theory (OT).
     The lenient composition of upper and lower is defined as follows:
     upper .O. lower = [[upper .o. lower] .P. upper]
     where .0. is the lenient compose operator,. .o. is ordinary
     composition and .P. is priority union.
     To make sense of this, think of upper as a transducer that maps
     each of the strings of the input language into all of its
     possible realization. In other words, upper is the composition
     of the input language with GEN. The lower network represents
     a constraint language that rules out some, maybe all of the
     outputs. The result of the lenient composition is a network that
     maps each input string to the outputs that meet the constraint
     if there are any, eliminating the outputs that violate the
     constraint. However, if none of the outputs of a given input
     meet the constraint, all of them remain. That is, lenient
     composition guarantees that every input has outputs. A set
     of ranked OT constraints can be implemented as a cascade of
     lenient compositions with the most higly ranked constraint on
     the top of the cascade. */

  NETptr close_sigma(NETptr net, ALPHABETptr new_symbols,
  		     int copy_p, int minimize_p);
  /* Error handling */

  void set_error_function(void (*fn)(const char *message,
				     const char *function_name,
                                     int code));

  void set_warning_function(void (*fn)(const char *message,
				       const char *function_name,
                                       int code));

  /*****************
   * TOKENIZER
   *****************/

  /* A tokenizer is an object that applies a tokenizing transducer to
     a string returning a network containing one or more possible
     tokenization of a section of the input string. For example, "Dr."
     could be a single token, an abbreviation of a title as in
     "Dr. No." It could also be another kind of single token, an
     abbreviaton "drive" as in "Mulholland Dr." A sentence final
     abbreviation adds to the ambiguity because it loses the final
     period in front of a sentence-terminating period, as in "We met
     at Mulholland Dr."  A tokenizer applies the tokenizer network to
     a string or a stream in breadth-first mode pursuing all
     alternatives in parallel.  At pinch points where all the
     alternative paths come together into single state, it returns a
     network representing all the possible tokenizations of the input
     string up to that point. */

  typedef struct TOKENIZER TOKtype, *TOKptr;
  /* The data structure for a tokenizer. */
||#

#+test ;; :string does not work. Why?
(defcfun "new_tokenizer" :pointer
  (token-fst :pointer)
  (in-string :string)
  (in-stream :ulong)
  (token-boundary :uint)
  (fail-token :uint)
  (cfsm-cntxt :pointer))

(defcfun "new_tokenizer" :pointer
  (token-fst :pointer)
  (in-string :pointer)
  (in-stream :ulong)
  (token-boundary :uint)
  (fail-token :uint)
  (cfsm-cntxt :pointer))

(defcfun "make_tokenizer" :pointer
  (fst-file :string)
  (in-string :string)
  (in-file :string)
  (token-boundary :string)
  (fail-token :string)
  (cfsm-cntxt :pointer))

(defcfun ("make_tokenizer" make-tokenizer1) :pointer
  (fst-file :string)
  (in-string :pointer)
  (in-file :string)
  (token-boundary :string)
  (fail-token :string)
  (cfsm-cntxt :pointer))

(defcfun "next_token_net" :pointer
  (tok :pointer))

(defcfun "free_tokenizer" :void
  (tok :pointer))


;;;  /*****************
;;;   *  PAGE
;;;   *****************/



#||
  typedef struct PAGE_OBJECT {
    int line_pos;
    int cur_pos;
    int line_no;
    int indent;
    int rm;
    int size;
    char *string;
    char *eol_string;
    char indent_char;
  } PAGEtype, *PAGEptr;

  /* Page objects are for storing formatted output. They are conceived
     as a sequence of lines with indentation and a right margin. The
     page writing routines keep track of the line position and insert
     an eol_string before the right margin is exceeded. The default
     eol_string is the default eol_string of the CFSM_CONTEXT, "\n".
     The size of the page grows as needed. */

#define PAGE_line_pos(X)            (X)->line_pos
#define PAGE_cur_pos(X)             (X)->cur_pos
#define PAGE_line_no(X)             (X)->line_no
#define PAGE_indent(X)              (X)->indent
  /* PAGE_rm is the width of the page in columns. If PAGE_rm is -1, the page
     has no right margin. */
#define PAGE_rm(X)                  (X)->rm
#define PAGE_size(X)                (X)->size
#define PAGE_string(X)              (X)->string
#define PAGE_eol_string(X)          (X)->eol_string
#define PAGE_indent_char(X)         (X)->indent_char

  int watch_margin(PAGEptr page, int next_size);
  /* Inserts an eol string, if adding next_size to the line position
     exceeds the right margin of the page. */

  int int_print_length(intptr_t i);
  /* Returns the number of digits in the print representation of an integer. */

  int label_length(LABELptr label, int escape_p, int bytes_or_chars);
  /* Returns the number of bytes or the number of characters in the print
     representation of the label. If bytes_or_chars is NUM_BYTES, the
     function returns the number of bytes. If the character encoding
     is UTF-8, NUM_BYTES may be larger than NUM_CHARS, the number of
     actual unicode characters of the label. If escape_p is ESCAPE,
     certain symbols such as newline characters are measured with
     surrounding double quotes. */

  PAGEptr new_page(void);
  /* Returns a new page with IY_INDENT indentation and IY_RIGHT_MARGIN
     as the right margin. The values of these macros are determined by
     the CFSM_CONTEXT. See below. */

  PAGEptr make_page(int size, int indent, int rm);
  /* Returns a new page with specified settings. */

  void free_page(PAGEptr page);
  /* Reclaims the page. */

  void reset_page(PAGEptr page);
  /* Resets the position and line coounters to zero. */
||#

(defcfun "new_page" page
  )

(defcfun "free_page" :void
  (page page))

(defcfun "reset_page" :void
  (page page))

(defcfun "print_page" :void
  (page page)
  (stream :ulong))

#||
  void print_page(PAGEptr page, FILE * stream);
  /* Prints the page into the stream. */

  void new_page_line(PAGEptr page);
  /* Inserts the eol_string at the current position on the page. */

  void char_to_page(char c, PAGEptr page);
  /* Appends the character to the page. */

  void int_to_page(intptr_t i, int watch_rm, PAGEptr page);
  /* Writes the integer to the page. If the second argument is WATCH_RM,
     an eol_string is inserted first if needed to avoid exceeding the
     right margin. If the second argument is DONT_WATCH_RM, the
     digits are written without watching the margin. */

  void float_to_page(float f, int watch_rm, PAGEptr page);
  /* Writes a floating point number to the page. */

  void spaces_to_page(int n, int watch_rm, PAGEptr page);
  /* Writes n spaces to the page. */

  void string_to_page(char *str, int watch_rm, PAGEptr page);
  /* Writes a C string to the page. */

  void fat_string_to_page(FAT_STR fs, PAGEptr page);
  /* Writes a fat string to the page. */

  void fat_string_to_page_esc(FAT_STR fs, const char *esc, PAGEptr page);
  /* Writes a fat string to the page. Characters on the esc list are
     printed with escapes. */

  void symbol_to_page(FAT_STR name, PAGEptr page);
  /* Writes a symbol name to the page with escapes for the following
     characters: '0' (literal zero), '?' (literal question mark), '%'
   (literal percent sign), ' ', '\t', '\n'. */

  void label_to_page(id_type id, int escape_p, int watch_rm, PAGEptr page);
  /* Writes a label to the page, either a single symbol or a pair of
     symbols separated fy a colon. */

  PAGEptr labels_to_page(NETptr net, PAGEptr page);
  /* Writes the label alphabet of the network to the page and returns
     the page. If the page argument is NULL, a new page created. */

  PAGEptr sigma_to_page(NETptr net, PAGEptr page);
  /* Writes the sigma alphabet of the network to the page and returns
     the page. In the verbose mode, the function of any flag diacritic
     in the sigma is described. If the page argument is NULL, a new page
     is created. */

  PAGEptr network_to_page(NETptr net, PAGEptr page);
  /* Writes the states and arcs of the network to the page and returns
     the page. If the page argument is NULL, a new page is created. */
||#

(defcfun "words_to_page" page
  (net :pointer)
  (side :int)
  (escape-p :int)
  (page page))

#||
  PAGEptr words_to_page(NETptr net, int side, int escape_p, PAGEptr page);
  /* Writes paths of the network to the page and returns the page. If
     the page argument is NULL, a new page is created. The side
     argument can be UPPER, LOWER, or BOTH. If the network is
     circular, a loop is traversed just once and the site of the loop
     is marked with three dots.  The output format is contolled by the
     macros IY_OBEY_FLAGS, IY_SHOW_FLAGS and IY_PRINT_SPACE, and
     IY_PRINT_PAIRS. See the explanations below. */

  PAGEptr random_words_to_page(NETptr net, int side, int n, PAGEptr page);
  /* Writes n random paths of the network to the page and returns the
     page. If the page argument is NULL, a new page is created. The
     side argument can be UPPER, LOWER, or BOTH. If the network is
     acyclic the algorithm counts the number of paths and picks one at
     random. If the network is cyclic, the algorithm chooses an
     outgoing arc at random and decides randomly to stop or to
     continue at a final state. The output format is contolled by the
     macros IY_OBEY_FLAGS, IY_SHOW_FLAGS and IY_PRINT_SPACE, and
     IY_PRINT_PAIRS. See the explanations below. */

  int alphabet_to_page(ALPHABETptr alph, PAGEptr page);
  /* Writes the alphabet to the page. Returns the number of items
     in the alphabet. */

  PAGEptr flags_to_page(NETptr net, PAGEptr page);
  /* Writes the network status flags to the page and returns the
     page. For example, Flags: deterministic, pruned, minimized,
     epsilon_free, loop_free. If the page argument is NULL, a new page
     is created. */

  PAGEptr properties_to_page(NETptr net, PAGEptr page);
  /* Writes the network property list to the page. and returns the
     page. If the page argument is NULL, a new page is created. */

  PAGEptr net_size_to_page(NETptr net, PAGEptr page);
  /* Writes the size of the network to the page and returns the
     page. For example, 660 bytes. 4 states, 3 arcs, 1 path. Label
     Map: Default. If the page argument is NULL, a new page is
     created. */

  PAGEptr time_to_page(intptr_t start, intptr_t end, PAGEptr page);
  /* Prints the difference between end and start times to the page in
     terms of seconds, minutes, and hours. and returns the page. If
     the page argument is NULL, a new page is created. */

  PAGEptr label_vector_to_page(LAB_VECTORptr lab_vect, PAGEptr page,
                               int escape_p, char *sep);
  /* Prints the labels corresponding to the label IDs in the label
     vector to the page and returns the page. If the page argument is
     NULL, a new page is created. */

  PAGEptr symbol_list_to_page(char *name, PAGEptr page);
  /* Writes the members of the list defined as name to the page and
     returns the page as the value, or NULL if an error occurs.
     If the page argument is NULL, a new page is creaed. */

  PAGEptr file_info_to_page(PAGEptr page);
  /* Writes the information to the page about the last network file
     that was either loaded or saved and returns the page. If the page
     argument is NULL a new page is created(). */

  PAGEptr storage_info_to_page(PAGEptr page);
  /* Writes the information to the page about the storage used
     for states, arcs, and other managed data structures.
     If the page argument is NULL a new page is created(). */

  int longest_string_to_page(NETptr net, int side, PAGEptr page);
  /* Writes to the page the longest string in the network, that is the
     string on the longest non-looping path from the start state to a
     final state. The side must be UPPER or LOWER.  Epsilons are
     ignored.  Returns the length of the string on success, -1 on
     error. Not implemented for vectorized or compacted networks. */

  int shortest_string_to_page(NETptr net, int side, PAGEptr page);
  /* Writes to the page the shortest string in the network, that is
     the string on the shortest path from a start state to a final
     state. The side must be UPPER or LOWER.  Epsilons are ignored.
     Returns the length of the string on success, -1 on error.  Not
     implemented for vectorized or compacted networks. */

  typedef struct TALLY {
    id_type label;
    uintptr_t freq;
  } TALLYcell, *TALLYptr;

||#


:eof
