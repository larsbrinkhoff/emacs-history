# 1 "filelock.c"
 


















# 1 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/types.h" 1 3
 










 



 


 


 


typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		 











typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[15];		 
} label_t;

typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef	u_long	ino_t;
typedef	long	swblk_t;


typedef unsigned long int size_t;

typedef	long	time_t;
typedef	short	dev_t;
typedef	long	off_t;
typedef	u_short	uid_t;
typedef	u_short	gid_t;
typedef	u_long	fixpt_t;
typedef	long	key_t;
typedef	long	paddr_t;	 


 









typedef long	fd_mask;





typedef	struct fd_set {
	fd_mask	fds_bits[((( 256  )+((  (sizeof(fd_mask) * 8		)	 )-1))/(  (sizeof(fd_mask) * 8		)	 )) ];
} fd_set;













# 20 "filelock.c" 2

# 1 "/usr/include/sys/stat.h" 1 3
 








struct	stat
{
	dev_t	st_dev;
	ino_t	st_ino;
	unsigned short st_mode;
	short	st_nlink;
	uid_t	st_uid;
	gid_t	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	int	st_spare1;
	time_t	st_mtime;
	int	st_spare2;
	time_t	st_ctime;
	int	st_spare3;
	long	st_blksize;
	long	st_blocks;
	long	st_spare4[2];
};















# 21 "filelock.c" 2

# 1 "config.h" 1
 
 




















 






 


 



 


 


  
 


 



 
 

 




 




 



 










 

 

 


 





 
 
 

 
 
 

 
 

 


 






 








# 1 "s/bsd4-3.h" 1
 



















 




 









 




 



 

 




 



 





 
 





 





 



 



 




 

 



 







 


 

 






 





 



 



 




 


# 122 "config.h" 2


 








# 1 "m/hp9000s300.h" 1
 



















 













 
# 49 "m/hp9000s300.h"


 

 

 


 

 








 

 





 


 

 


 

 






 
 



 







 



 

 






 

 





 



















# 232 "m/hp9000s300.h"

# 133 "config.h" 2


 










 







 



 
 

 
 

 






 






 



 




 






# 205 "config.h"


 






extern char *getenv ();



# 22 "filelock.c" 2





# 1 "/usr/include/pwd.h" 1 3
 

struct	passwd {  
	char	*pw_name;
	char	*pw_passwd;
	int	pw_uid;
	int	pw_gid;
	int	pw_quota;
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
};

struct passwd *getpwent(), *getpwuid(), *getpwnam();
# 27 "filelock.c" 2



# 1 "/usr/include/errno.h" 1 3
 




# 1 "/usr/include/sys/errno.h" 1 3
 








 




































 



 




 

	 













	 













	 



 




 




 



 




 


# 6 "/usr/include/errno.h" 2 3


extern int errno;
# 30 "filelock.c" 2

# 1 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/file.h" 1 3
 









# 38 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/file.h" 3

# 1 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/fcntl.h" 1 3
 











 













 








 











 





 
 





 





 
struct flock {
        short   l_type;		 
        short   l_whence;	 
        long    l_start;	 
        long    l_len;           
        short   l_pid;		 
        short   l_xxx;		 
};


# 39 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/file.h" 2 3



 










 



 



 







 







 






# 93 "/usr/local/lib/gcc-lib/m68k-hp-bsd/2.4.3/include/sys/file.h" 3

# 31 "filelock.c" 2





# 1 "lisp.h" 1
 



















 

 

enum Lisp_Type
  {
     
    Lisp_Int,

     
    Lisp_Symbol,

     
    Lisp_Marker,

     

    Lisp_String,

     

    Lisp_Vector,

     
    Lisp_Cons,

     


    Lisp_Compiled,

     
    Lisp_Buffer,

     


    Lisp_Subr,

     


    Lisp_Internal,

     




    Lisp_Intfwd,

     



    Lisp_Boolfwd,

     

    Lisp_Process,

     




    Lisp_Objfwd,


     



    Lisp_Frame,


     



    Lisp_Internal_Stream,

     






























    Lisp_Buffer_Local_Value,

     



    Lisp_Some_Buffer_Local_Value,


     

    Lisp_Buffer_Objfwd,

     

    Lisp_Void,

     

    Lisp_Window,

     
    Lisp_Window_Configuration,


    Lisp_Float,


     





    Lisp_Overlay
  };

# 237 "lisp.h"



 






 
















 





 











 



 





 











































 










 























# 408 "lisp.h"
































 

struct interval
{
   

  unsigned int total_length;	 
  unsigned int position;	 
  struct interval *left;	 
  struct interval *right;	 
  struct interval *parent;	 


   



  unsigned char write_protect;	     
  unsigned char visible;	     
  unsigned char front_sticky;	     

  unsigned char rear_sticky;	     

  int  plist;		     
};

typedef struct interval *INTERVAL;

 




 



 



# 491 "lisp.h"


 

struct Lisp_Cons
  {
    int  car, cdr;
  };

 
 

struct Lisp_Buffer_Cons
  {
    int  car, cdr;
    struct buffer *buffer;
    int bufpos;
  };

 

struct Lisp_String
  {
    int size;
    INTERVAL intervals; 		 
    unsigned char data[1];
  };

struct Lisp_Vector
  {
    int size;
    struct Lisp_Vector *next;
    int  contents[1];
  };

 

struct Lisp_Symbol
  {
    struct Lisp_String *name;
    int  value;
    int  function;
    int  plist;
    struct Lisp_Symbol *next;	 
  };

struct Lisp_Subr
  {
    int  (*function) ();
    short min_args, max_args;
    char *symbol_name;
    char *prompt;
    char *doc;
  };

 

struct Lisp_Marker
  {
    struct buffer *buffer;
    int  chain;
    int bufpos;
    int modified;
  };


 
struct Lisp_Float
  {
    int  type;		 

    double data;  
  };


 



typedef unsigned char UCHAR;


 








 











 

 








 


 


 



 






















































 































































 




 















































 

extern void defsubr ();




extern void defvar_lisp ();
extern void defvar_bool ();
extern void defvar_int ();

 









 

 











struct specbinding
  {
    int  symbol, old_value;
    int  (*func) ();
    int  unused;		 
  };

extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern int specpdl_size;

 
struct handler
  {
     
    int  handler;
    int  var;

     
    struct catchtag *tag;

     
    struct handler *next;
  };

extern struct handler *handlerlist;

extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

 

extern char *stack_bottom;

 





 



 




 





 



 





 



 



 



 



extern int  Vascii_downcase_table, Vascii_upcase_table;

 

extern int consing_since_gc;

 

extern int gc_cons_threshold;

 

 









extern struct gcpro *gcprolist;

struct gcpro
  {
    struct gcpro *next;
    int  *var;		 
    int nvars;			 
  };























 

void staticpro();
  


 











 
extern int  Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
extern int  Qerror_conditions, Qerror_message, Qtop_level;
extern int  Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
extern int  Qvoid_variable, Qvoid_function;
extern int  Qsetting_constant, Qinvalid_read_syntax;
extern int  Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern int  Qend_of_file, Qarith_error;
extern int  Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;

extern int  Qrange_error, Qdomain_error, Qsingularity_error;
extern int  Qoverflow_error, Qunderflow_error;

extern int  Qintegerp, Qnumberp, Qnatnump, Qsymbolp, Qlistp, Qconsp;
extern int  Qstringp, Qarrayp, Qsequencep, Qbufferp;
extern int  Qchar_or_string_p, Qmarkerp, Qvectorp;
extern int  Qinteger_or_marker_p, Qnumber_or_marker_p;
extern int  Qboundp, Qfboundp;
extern int  Qbuffer_or_string_p;
extern int  Qcdr;


extern int  Qfloatp, Qinteger_or_floatp, Qinteger_or_float_or_marker_p;


extern int  Qframep;

extern int  Feq (), Fnull (), Flistp (), Fconsp (), Fatom (), Fnlistp ();
extern int  Fintegerp (), Fnatnump (), Fsymbolp ();
extern int  Fvectorp (), Fstringp (), Farrayp (), Fsequencep ();
extern int  Fbufferp (), Fmarkerp (), Fsubrp (), Fchar_or_string_p ();
extern int  Finteger_or_marker_p ();

extern int  Ffloatp(), Finteger_or_floatp();
extern int  Finteger_or_float_or_marker_p(), Ftruncate();


extern int  Fcar (), Fcar_safe(), Fcdr (), Fcdr_safe();
extern int  Fsetcar (), Fsetcdr ();
extern int  Fboundp (), Ffboundp (), Fmakunbound (), Ffmakunbound ();
extern int  Fsymbol_function (), Fsymbol_plist (), Fsymbol_name ();
extern int  indirect_function (), Findirect_function ();
extern int  Ffset (), Fsetplist ();
extern int  Fsymbol_value (), find_symbol_value (), Fset ();
extern int  Fdefault_value (), Fset_default (), Fdefault_boundp ();

extern int  Faref (), Faset (), Farray_length ();

extern int  Fstring_to_number (), Fnumber_to_string ();
extern int  Feqlsign (), Fgtr (), Flss (), Fgeq (), Fleq ();
extern int  Fneq (), Fzerop ();
extern int  Fplus (), Fminus (), Ftimes (), Fquo (), Frem ();
extern int  Fmax (), Fmin ();
extern int  Flogand (), Flogior (), Flogxor (), Flognot ();
extern int  Flsh (), Fash ();

extern int  Fadd1 (), Fsub1 ();

extern int  make_number ();
extern int    long_to_cons ();
extern unsigned long cons_to_long ();
extern void args_out_of_range ();
extern void args_out_of_range_3 ();
extern int  wrong_type_argument ();

extern int  Ffloat_to_int(), Fint_to_float();
extern double extract_float();
extern int  make_float ();
extern int  Ffloat ();


 
extern int  Qstring_lessp;
extern int  Vfeatures;
extern int  Fidentity (), Frandom ();
extern int  Flength ();
extern int  Fappend (), Fconcat (), Fvconcat (), Fcopy_sequence ();
extern int  Fsubstring ();
extern int  Fnth (), Fnthcdr (), Fmemq (), Fassq (), Fassoc ();
extern int  Frassq (), Fdelq (), Fsort ();
extern int  Freverse (), Fnreverse (), Fget (), Fput (), Fequal ();
extern int  Ffillarray (), Fnconc (), Fmapcar (), Fmapconcat ();
extern int  Fy_or_n_p (), do_yes_or_no_p ();
extern int  Ffeaturep (), Frequire () , Fprovide ();
extern int  concat2 (), nconc2 ();
extern int  assq_no_quit ();
extern int  Fcopy_alist ();

 
extern int  Vpurify_flag;
extern int  Fcons (), Flist(), Fmake_list ();
extern int  Fmake_vector (), Fvector (), Fmake_symbol (), Fmake_marker ();
extern int  Fmake_string (), build_string (), make_string ();
extern int  make_event_array (), make_uninit_string ();
extern int  Fpurecopy (), make_pure_string ();
extern int  pure_cons (), make_pure_vector ();
extern int  Fgarbage_collect ();
extern int  Fmake_byte_code ();

 
extern int  Vprin1_to_string_buffer;
extern int  Fprin1 (), Fprin1_to_string (), Fprinc ();
extern int  Fterpri (), Fprint ();
extern int  Vstandard_output, Qstandard_output;
extern int  Qexternal_debugging_output;
extern void temp_output_buffer_setup (), temp_output_buffer_show ();
extern int print_level, print_escape_newlines;
extern int  Qprint_escape_newlines;

 
extern int  Qvariable_documentation, Qstandard_input;
extern int  Vobarray, Vstandard_input;
extern int  Fread (), Fread_from_string ();
extern int  Fintern (), Fintern_soft (), Fload ();
extern int  Fget_file_char (), Fread_char ();
extern int  read_filtered_event ();
extern int  Feval_current_buffer (), Feval_region ();
extern int  intern (), oblookup ();


extern int  Vcurrent_load_list;
extern int  Vload_history;

 
extern int  Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern int  Vinhibit_quit, Qinhibit_quit, Vquit_flag;
extern int  Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
extern int  Vautoload_queue;
extern int  Vrun_hooks;
extern int  Fand (), For (), Fif (), Fprogn (), Fprog1 (), Fprog2 ();
extern int  Fsetq (), Fquote ();
extern int  Fuser_variable_p (), Finteractive_p ();
extern int  Fdefun (), Flet (), FletX (), Fwhile ();
extern int  Fcatch (), Fthrow (), Funwind_protect ();
extern int  Fcondition_case (), Fsignal ();
extern int  Ffunction_type (), Fautoload (), Fcommandp ();
extern int  Feval (), Fapply (), Ffuncall ();
extern int  Fglobal_set (), Fglobal_value (), Fbacktrace ();
extern int  apply1 (), call0 (), call1 (), call2 (), call3 ();
extern int  apply_lambda ();
extern int  internal_catch ();
extern int  internal_condition_case ();
extern int  unbind_to ();
extern void error ();
extern int  un_autoload ();

 
extern int  Vprefix_arg, Qminus, Vcurrent_prefix_arg;
extern int  Fgoto_char ();
extern int  Fpoint_min_marker (), Fpoint_max_marker ();
extern int  Fpoint_min (), Fpoint_max ();
extern int  Fpoint (), Fpoint_marker (), Fmark_marker ();
extern int  Ffollowing_char (), Fprevious_char (), Fchar_after ();
extern int  Finsert ();
extern int  Feolp (), Feobp (), Fbolp (), Fbobp ();
extern int  Fformat (), format1 ();
extern int  make_buffer_string (), Fbuffer_substring ();
extern int  Fbuffer_string ();
extern int  Fstring_equal (), Fstring_lessp (), Fbuffer_substring_lessp ();
extern int  save_excursion_save (), save_restriction_save ();
extern int  save_excursion_restore (), save_restriction_restore ();
extern int  Fchar_to_string ();

 
extern int  Vbuffer_alist, Vinhibit_read_only;
extern int  Fget_buffer (), Fget_buffer_create (), Fset_buffer ();
extern int  Fbarf_if_buffer_read_only ();
extern int  Fcurrent_buffer (), Fswitch_to_buffer (), Fpop_to_buffer ();
extern int  Fother_buffer ();
extern int  Qoverlayp;
extern struct buffer *all_buffers;

 

extern int  Fmarker_position (), Fmarker_buffer ();
extern int  Fcopy_marker ();

 

extern int  Qfile_error;
extern int  Ffind_file_name_handler ();
extern int  Ffile_name_as_directory ();
extern int  Fexpand_file_name (), Ffile_name_nondirectory ();
extern int  Fsubstitute_in_file_name ();
extern int  Ffile_symlink_p ();
extern int  Fverify_visited_file_modtime ();
extern int  Ffile_exists_p ();
extern int  Fdirectory_file_name ();
extern int  Ffile_name_directory ();
extern int  expand_and_dir_to_file ();
extern int  Ffile_accessible_directory_p ();

 

extern int  Vfundamental_mode_abbrev_table;

 
extern int  Fstring_match ();
extern int  Fscan_buffer ();

 

extern int  last_minibuf_string;
extern int  read_minibuf (), Fcompleting_read ();
extern int  Fread_from_minibuffer ();
extern int  Fread_variable (), Fread_buffer (), Fread_key_sequence ();
extern int  Fread_minibuffer (), Feval_minibuffer ();
extern int  Fread_string (), Fread_file_name ();
extern int  Fread_no_blanks_input ();

 

extern int  Vcommand_history;
extern int  Qcall_interactively;
extern int  Fcall_interactively ();
extern int  Fprefix_numeric_value ();

 

extern int  Fdowncase (), Fupcase (), Fcapitalize ();

 

extern int  Qdisabled;
extern int  Vhelp_form, Vtop_level;
extern int  Fdiscard_input (), Frecursive_edit ();
extern int  Fcommand_execute (), Finput_pending_p ();
extern int  Qvertical_scroll_bar;

 

extern int  Qkeymap;
extern int  current_global_map;
extern int  Fkey_description (), Fsingle_key_description ();
extern int  Fwhere_is_internal ();
extern int  access_keymap (), store_in_keymap ();
extern int  get_keyelt (), get_keymap();

 
extern int  Fvertical_motion (), Findent_to (), Fcurrent_column ();

 
extern int  Qwindowp, Qwindow_live_p;
extern int  Fget_buffer_window ();
extern int  Fsave_window_excursion ();
extern int  Fset_window_configuration (), Fcurrent_window_configuration ();
extern int  Fcoordinates_in_window_p ();
extern int  Fwindow_at ();
extern int window_internal_height (), window_internal_width ();

 
extern int  Fframep ();
extern int  Fselect_frame ();
extern int  Ffocus_frame ();
extern int  Funfocus_frame ();
extern int  Fselected_frame ();
extern int  Fwindow_frame ();
extern int  Fframe_root_window ();
extern int  Fframe_selected_window ();
extern int  Fframe_list ();
extern int  Fnext_frame ();
extern int  Fdelete_frame ();
extern int  Fread_mouse_position ();
extern int  Fset_mouse_position ();
extern int  Fmake_frame_visible ();
extern int  Fmake_frame_invisible ();
extern int  Ficonify_frame ();
extern int  Fdeiconify_frame ();
extern int  Fframe_visible_p ();
extern int  Fvisible_frame_list ();
extern int  Fframe_parameters ();
extern int  Fmodify_frame_parameters ();
extern int  Fframe_pixel_size ();
extern int  Fframe_height ();
extern int  Fframe_width ();
extern int  Fset_frame_height ();
extern int  Fset_frame_width ();
extern int  Fset_frame_size ();
extern int  Fset_frame_position ();




 
extern int  decode_env_path ();
extern int  Vinvocation_name;
void shut_down_emacs (   );
 
extern int noninteractive;
 
extern int inhibit_window_system;

 
extern int  Fget_process (), Fget_buffer_process (), Fprocessp ();
extern int  Fprocess_status (), Fkill_process ();

 
extern int  Vexec_path, Vexec_directory, Vdata_directory;

 
extern int  Vdoc_file_name;
extern int  Fsubstitute_command_keys ();
extern int  Fdocumentation (), Fdocumentation_property ();

 
extern int  Qbytecode;
extern int  Fbyte_code ();

 
extern int  Qexecute_kbd_macro;
extern int  Fexecute_kbd_macro ();

 
extern int  Fundo_boundary ();
extern int  truncate_undo_list ();

 

extern int initialized;

extern int immediate_quit;	     

extern void debugger ();

extern char *getenv (), *ctime (), *getwd ();
extern long *xmalloc (), *xrealloc ();
extern void xfree ();

extern char *egetenv ();
 
# 36 "filelock.c" 2

# 1 "paths.h" 1
 

 



 





 






 




 





 




# 37 "filelock.c" 2

# 1 "buffer.h" 1
 



































  


  


 
 


  


  


  


 


 


  


  


  


  


 


  


 


 


  


  


  


  


  


  


 



 


 


 




 


 





 





struct buffer_text
  {
    unsigned char *beg;		     
    int begv;			 
    int pt;			 
    int gpt;			 
    int zv;			 
    int z;			 
    int gap_size;		 
    int modiff;			 



  };

struct buffer
  {
     





     
    struct buffer_text text;
     


    struct buffer *next;
     

    int local_var_flags;
     
    int save_modified;
     



    int modtime;
     
    int auto_save_modified;
     

    int last_window_start;

     
    INTERVAL intervals; 

     



     




    int  markers;


     


     
    int  name;
     
     
    int  filename;
     
    int  directory;
     



     
    int  backed_up;
     
    int  save_length;
     
    int  auto_save_file_name;
     
    int  read_only;
     
    int  mark;

     

    int  local_var_alist;


     
    int  major_mode;
     
    int  mode_name;
     
    int  mode_line_format;

     
    int  keymap;
     
    int  abbrev_table;
     
    int  syntax_table;

     
     

    int  case_fold_search;
    int  tab_width;
    int  fill_column;
    int  left_margin;
     
    int  auto_fill_function;

     
    int  downcase_table;
     
    int  upcase_table;

     
    int  truncate_lines;
     
    int  ctl_arrow;
     

    int  selective_display;

     
    int  selective_display_ellipses;

     
    int  minor_modes;
     

    int  overwrite_mode;
     
    int  abbrev_mode;
     
    int  display_table;
     
    int  case_canon_table;
     
    int  case_eqv_table;
     

    int  undo_list;
     
    int  mark_active;

     

    int  overlays_before;

     

    int  overlays_after;

     
    int  overlay_center;
};

 

extern struct buffer *current_buffer;

 






extern struct buffer buffer_defaults;

 












extern struct buffer buffer_local_flags;

 



extern struct buffer buffer_local_symbols;

 






extern struct buffer buffer_local_types;

 



 


 







extern void reset_buffer ();

extern int  Fbuffer_name ();
extern int  Fget_file_buffer ();

 
extern int  Vbefore_change_function;
extern int  Vafter_change_function;
extern int  Vfirst_change_hook;
extern int  Qfirst_change_hook;

extern int  Vdeactivate_mark;
extern int  Vtransient_mark_mode;

 

 


 


 


 


 







 












# 38 "filelock.c" 2


extern int errno;

extern char *egetenv ();
extern char *strcpy ();


  
 







 
  
char *lock_path;

 



 

char *superlock_path;

 









fill_in_lock_short_file_name (lockfile, fn)
     register char *lockfile;
     register int  fn;
{
  register union
    {
      unsigned int  word [2];
      unsigned char byte [8];
    } crc;
  register unsigned char *p, new;
  
   


  crc.word[0] = crc.word[1] = 0;

  for (p = ((struct Lisp_String *) ((   fn   ) & ((1<<24 ) - 1) )  ) ->data; new = *p++; )
    {
      new += crc.byte[7];
      crc.byte[7] = crc.byte[6];
      crc.byte[6] = crc.byte[5] + new;
      crc.byte[5] = crc.byte[4];
      crc.byte[4] = crc.byte[3];
      crc.byte[3] = crc.byte[2] + new;
      crc.byte[2] = crc.byte[1];
      crc.byte[1] = crc.byte[0];
      crc.byte[0] = new;
    }
  sprintf (lockfile, "%s%.2x%.2x%.2x%.2x%.2x%.2x%.2x", lock_path,
	   crc.byte[0], crc.byte[1], crc.byte[2], crc.byte[3],
	   crc.byte[4], crc.byte[5], crc.byte[6]);
}

# 135 "filelock.c"


static int 
lock_file_owner_name (lfname)
     char *lfname;
{
  struct stat s;
  struct passwd *the_pw;
  extern struct passwd *getpwuid ();

  if (lstat (lfname, &s) == 0)
    the_pw = getpwuid (s.st_uid);
  return (the_pw == 0 ? Qnil : build_string (the_pw->pw_name));
}


 
















 

 

 




void
lock_file (fn)
     register int  fn;
{
  register int  attack;
  register char *lfname;

  ( lfname  = (char *) alloca (14 + strlen (lock_path) + 1), fill_in_lock_short_file_name ( lfname , (  fn ))) ;

   

  {
    register int  subject_buf = Fget_file_buffer (fn);
    if (! ((  subject_buf  )  == ( Qnil ) ) 
	&&  ((  Fverify_visited_file_modtime (subject_buf)  )  == ( Qnil ) ) 
	&& ! ((  Ffile_exists_p (fn)  )  == ( Qnil ) ) )
      call1 (intern ("ask-user-about-supersession-threat"), fn);
  }

   
  if (lock_if_free (lfname) <= 0)
     
    return;

   
  attack = call2 (intern ("ask-user-about-lock"), fn,
		  lock_file_owner_name (lfname));
  if (! ((  attack  )  == ( Qnil ) ) )
     
    {
      lock_superlock (lfname);
      lock_file_1 (lfname, 1 ) ;
      unlink (superlock_path);
      return;
    }
   
}

 




int
lock_file_1 (lfname, mode)
     int mode; char *lfname; 
{
  register int fd;
  char buf[20];

  if ((fd = open (lfname, mode, 0666)) >= 0)
    {



      fchmod (fd, 0666);

      sprintf (buf, "%d ", getpid ());
      write (fd, buf, strlen (buf));
      close (fd);
      return 1;
    }
  else
    return 0;
}

 




int
lock_if_free (lfname)
     register char *lfname; 
{
  register int clasher;

  while (lock_file_1 (lfname, 1  | 		04000			 | 		01000			) == 0)
    {
      if (errno != 	17		)
	return -1;
      clasher = current_lock_owner (lfname);
      if (clasher != 0)
	if (clasher != getpid ())
	  return (clasher);
	else return (0);
       
    }
  return 0;
}

 



int
current_lock_owner (lfname)
     char *lfname;
{
  int owner = current_lock_owner_1 (lfname);
  if (owner == 0 && errno == 	2		)
    return (0);
   
  if (owner != 0 && (kill (owner, 0) >= 0 || errno == 	1		))
    return (owner);
  if (unlink (lfname) < 0)
    return (-1);
  return (0);
}

int
current_lock_owner_1 (lfname)
     char *lfname;
{
  register int fd;
  char buf[20];
  int tem;

  fd = open (lfname, 0 , 0666);
  if (fd < 0)
    return 0;
  tem = read (fd, buf, sizeof buf);
  close (fd);
  return (tem <= 0 ? 0 : atoi (buf));
}


void
unlock_file (fn)
     register int  fn;
{
  register char *lfname;

  ( lfname  = (char *) alloca (14 + strlen (lock_path) + 1), fill_in_lock_short_file_name ( lfname , (  fn ))) ;

  lock_superlock (lfname);

  if (current_lock_owner_1 (lfname) == getpid ())
    unlink (lfname);

  unlink (superlock_path);
}

lock_superlock (lfname)
     char *lfname;
{
  register int i, fd;

  for (i = -20; i < 0 && (fd = open (superlock_path,
				     1  | 		04000			 | 		01000			, 0666)) < 0;
       i++)
    {
      if (errno != 	17		)
	return;
      sleep (1);
    }
  if (fd >= 0)
    {



      fchmod (fd, 0666);

      write (fd, lfname, strlen (lfname));
      close (fd);
    }
}

void
unlock_all_files ()
{
  register int  tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; ((enum Lisp_Type) ((( tail ) >> 24 ) & ((1<<7 ) - 1) ))  == Lisp_Cons;
       tail = ((struct Lisp_Cons *) ((   tail   ) & ((1<<24 ) - 1) )  ) ->cdr)
    {
      b = ((struct buffer *) ((   ((struct Lisp_Cons *) ((   ((struct Lisp_Cons *) ((   tail   ) & ((1<<24 ) - 1) )  ) ->car   ) & ((1<<24 ) - 1) )  ) ->cdr   ) & ((1<<24 ) - 1) )  ) ;
      if (((enum Lisp_Type) (( b->filename ) >> 24 ))  == Lisp_String &&
	  b->save_modified < (( b )->text.modiff) )
	unlock_file (b->filename);
    }
}


int    Flock_buffer  (); struct Lisp_Subr   Slock_buffer  = {  Flock_buffer ,  
  0 ,   1 ,  "lock-buffer" ,   0 , 0}; int    Flock_buffer  



  (fn)
     int  fn;
{
  if ( ((  fn  )  == ( Qnil ) ) )
    fn = current_buffer->filename;
  else
    do { if (((enum Lisp_Type) (( ( fn ) ) >> 24 ))  != Lisp_String)  fn  = wrong_type_argument (Qstringp, ( fn )); } while (0) ;
  if (current_buffer->save_modified < (current_buffer->text.modiff) 
      && ! ((  fn  )  == ( Qnil ) ) )
    lock_file (fn);
  return Qnil;    
}

int    Funlock_buffer  (); struct Lisp_Subr   Sunlock_buffer  = {  Funlock_buffer ,  
  0 ,   0 ,  "unlock-buffer" ,   0 , 0}; int    Funlock_buffer  


  ()
{
  if (current_buffer->save_modified < (current_buffer->text.modiff)  &&
      ((enum Lisp_Type) (( current_buffer->filename ) >> 24 ))  == Lisp_String)
    unlock_file (current_buffer->filename);
  return Qnil;
}


 

unlock_buffer (buffer)
     struct buffer *buffer;
{
  if (buffer->save_modified < (( buffer )->text.modiff)  &&
      ((enum Lisp_Type) (( buffer->filename ) >> 24 ))  == Lisp_String)
    unlock_file (buffer->filename);
}

int    Ffile_locked_p  (); struct Lisp_Subr   Sfile_locked_p  = {  Ffile_locked_p ,   0 ,   1 ,  "file-locked-p" ,   0 , 0}; int    Ffile_locked_p  


  (fn)
  int  fn;
{
  register char *lfname;
  int owner;

  fn = Fexpand_file_name (fn, Qnil);

  ( lfname  = (char *) alloca (14 + strlen (lock_path) + 1), fill_in_lock_short_file_name ( lfname , (  fn ))) ;

  owner = current_lock_owner (lfname);
  if (owner <= 0)
    return (Qnil);
  else if (owner == getpid ())
    return (Qt);
  
  return (lock_file_owner_name (lfname));
}


 

init_filelock ()
{
  lock_path = egetenv ("EMACSLOCKDIR");
  if (! lock_path)
    lock_path = "/home/gd/gnu/emacs/19.0/lock/" ;

   
  if (lock_path[strlen (lock_path) - 1] != '/')
    {
      lock_path = strcpy ((char *) xmalloc (strlen (lock_path) + 2),
			  lock_path);
      strcat (lock_path, "/");
    }

  superlock_path = (char *) xmalloc ((strlen (lock_path)
				      + sizeof ("!!!SuperLock!!!" )));
  strcpy (superlock_path, lock_path);
  strcat (superlock_path, "!!!SuperLock!!!" );
}

syms_of_filelock ()
{
  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);
}


