/* Generated from tsthash.cbl by cobc version 1.0 patch level 0 */

#define  __USE_STRING_INLINES 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <libcob.h>

#define COB_SOURCE_FILE		"tsthash.cbl"
#define COB_PACKAGE_VERSION	"1.0"
#define COB_PATCH_LEVEL		0

static void
cob_decimal_set_int (cob_decimal *d, const int n)
{
	mpz_set_si (d->value, n);
	d->scale = 0;
}

/* function prototypes */
static int TST_2DHASH_ (const int);

int TST_2DHASH (void);


/* functions */

int
TST_2DHASH ()
{
  return TST_2DHASH_ (0);
}

/* end functions */

static int
TST_2DHASH_ (const int entry)
{

#include "tsthash.c.h"  /* local variables */

  static int initialized = 0;
  static cob_decimal d0;
  static cob_decimal d1;
  static cob_field *cob_user_parameters[COB_MAX_FIELD_PARAMS];
  static cob_module module = { NULL, NULL, NULL, NULL, cob_user_parameters, 0, '.', '$', ',', 1, 1, 1, 0};


  /* Files */

  static cob_file_key *k_HASH_FILE = NULL;
  static cob_file *h_HASH_FILE = NULL;
  static char h_HASH_FILE_status[4];



  /* perform frame stack */
  int frame_index;
  struct frame {
  	int perform_through;
  	void *return_address;
  } frame_stack[255];

  /* Start of function code */

  if (unlikely(entry < 0)) {
  	if (!initialized) {
  		return 0;
  	}
  	cob_close (h_HASH_FILE, 0, NULL);
  	mpz_clear (d0.value);
  	d0.scale = 0;
  	mpz_clear (d1.value);
  	d1.scale = 0;
  	initialized = 0;
  	return 0;
  }

  module.next = cob_current_module;
  cob_current_module = &module;

  if (unlikely(initialized == 0))
    {
      if (!cob_initialized) {
        cob_fatal_error (COB_FERROR_INITIALIZED);
      }
      cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);
      if (module.next)
        cob_set_cancel ((const char *)"TST-HASH", (void *)TST_2DHASH, (void *)TST_2DHASH_);
      /* initialize decimal numbers */
      cob_decimal_init (&d0);
      cob_decimal_init (&d1);

      (*(int *) (b_1)) = 0;
      (*(int *) (b_2)) = 0;
      (*(int *) (b_3)) = 0;
      memset (b_10, 0, 8);
      memset (b_10 + 8, 32, 24);
      memset (b_14, 0, 4);
      memcpy (b_15, "0123456Hartness, Ken       000005", 33);
      memset (b_15 + 33, 48, 4);
      memcpy (b_15 + 37, "0123457Neudorf, Diane      0000025", 34);
      memset (b_15 + 37 + 34, 48, 3);
      memcpy (b_15 + 74, "1234456Williams, Betty     000003", 33);
      memset (b_15 + 74 + 33, 48, 4);
      memcpy (b_15 + 111, "1234457Williams, Bob       000001", 33);
      memset (b_15 + 111 + 33, 48, 4);
      memcpy (b_15 + 148, "2345610Reed, Anne          000002", 33);
      memset (b_15 + 148 + 33, 48, 4);
      memcpy (b_15 + 185, "2345710Prouty, Carol       000002", 33);
      memset (b_15 + 185 + 33, 48, 4);
      memcpy (b_15 + 222, "3456789Arnold, Frank       000001", 33);
      memset (b_15 + 222 + 33, 48, 4);
      memcpy (b_15 + 259, "4567890Cockburn, Mary      0000001", 34);
      memset (b_15 + 259 + 34, 48, 3);
      memcpy (b_15 + 296, "5839000Thompson, Pat       0000000500", 37);
      memcpy (b_15 + 333, "6123000Holloway, Gail      0000002500", 37);
      memcpy (b_15 + 370, "8349000Sundman, Dawn       000001", 33);
      memset (b_15 + 370 + 33, 48, 4);
      memcpy (b_15 + 407, "8349001Sundman, Robert     0000025", 34);
      memset (b_15 + 407 + 34, 48, 3);
      *(unsigned char *)(b_33) = 0;
      memset (b_34, 0, 4);
      *(unsigned char *)(b_35) = 48;
      cob_move (&cob_zero, &f_40);

      if (!h_HASH_FILE)
      {
        h_HASH_FILE = cob_malloc (sizeof(cob_file));
      }
      if (!k_HASH_FILE)
      {
        k_HASH_FILE = cob_malloc (sizeof (cob_file_key) * 1);
      }
      k_HASH_FILE->field = &f_14;
      k_HASH_FILE->flag = 0;
      k_HASH_FILE->offset = 0;
      h_HASH_FILE->select_name = (const char *)"HASH-FILE";
      h_HASH_FILE->file_status = h_HASH_FILE_status;
      memset (h_HASH_FILE_status, '0', 2);
      h_HASH_FILE->assign = &c_1;
      h_HASH_FILE->record = &f_9;
      h_HASH_FILE->record_size = NULL;
      h_HASH_FILE->record_min = 35;
      h_HASH_FILE->record_max = 35;
      h_HASH_FILE->nkeys = 1;
      h_HASH_FILE->keys = k_HASH_FILE;
      h_HASH_FILE->file = NULL;
      h_HASH_FILE->organization = 2;
      h_HASH_FILE->access_mode = 3;
      h_HASH_FILE->lock_mode = 0;
      h_HASH_FILE->open_mode = 0;
      h_HASH_FILE->flag_optional = 1;
      h_HASH_FILE->last_open_mode = 0;
      h_HASH_FILE->special = 0;
      h_HASH_FILE->flag_nonexistent = 0;
      h_HASH_FILE->flag_end_of_file = 0;
      h_HASH_FILE->flag_begin_of_file = 0;
      h_HASH_FILE->flag_first_read = 0;
      h_HASH_FILE->flag_read_done = 0;
      h_HASH_FILE->flag_select_features = 0;
      h_HASH_FILE->flag_needs_nl = 0;
      h_HASH_FILE->flag_needs_top = 0;
      h_HASH_FILE->file_version = 0;

      initialized = 1;
    }

  /* initialize frame stack */
  frame_index = 0;
  frame_stack[0].perform_through = -1;

  /* initialize number of call params */
  (*(int *) (b_3))   = cob_call_params;
  cob_save_call_params = cob_call_params;

  goto l_2;

  /* PROCEDURE DIVISION */

  /* TST_2DHASH: */
  l_2:;

  /* MAIN SECTION: */

  /* MAIN: */
  /* tsthash.cbl:56: PERFORM */
  {
    /* PERFORM CREATE-TABLE THRU CREATE-TABLE */
    frame_index++;
    frame_stack[frame_index].perform_through = 5;
    frame_stack[frame_index].return_address = &&l_9;
    goto l_5;
    l_9:
    frame_index--;
  }
  /* tsthash.cbl:57: PERFORM */
  {
    /* PERFORM ECHO-TABLE THRU ECHO-TABLE */
    frame_index++;
    frame_stack[frame_index].perform_through = 7;
    frame_stack[frame_index].return_address = &&l_10;
    goto l_7;
    l_10:
    frame_index--;
  }
  /* tsthash.cbl:58: STOP */
  {
    cob_stop_run ((*(int *) (b_1)));
  }

  /* CREATE-TABLE: */
  l_5:;
  /* tsthash.cbl:61: DISPLAY */
  {
    cob_new_display (0, 1, 1, &c_2);
  }
  /* tsthash.cbl:62: OPEN */
  {
    cob_exception_code = 0;
    {
      cob_open (h_HASH_FILE, 3, 1, 0);
    }
    if (unlikely(cob_exception_code != 0))
      {
        /* PERFORM standard_error_handler THRU standard_error_handler */
        frame_index++;
        frame_stack[frame_index].perform_through = 1;
        frame_stack[frame_index].return_address = &&l_11;
        goto l_1;
        l_11:
        frame_index--;
      }
  }
  /* tsthash.cbl:63: MOVE */
  {
    cob_setswp_u32_binary (b_14, 1);
  }
  /* tsthash.cbl:64: DISPLAY */
  {
    cob_new_display (0, 1, 1, &c_3);
  }
  /* tsthash.cbl:65: READ */
  cob_exception_code = 0;
  {
    cob_read (h_HASH_FILE, &f_14, 0, 0);
  }
  if (unlikely(cob_exception_code == 0x0506))
    {
      {
        /* tsthash.cbl:67: MOVE */
        {
          cob_setswp_u32_binary (b_10, 23);
        }
        /* tsthash.cbl:68: MOVE */
        {
          cob_setswp_u32_binary (b_10 + 4, 32);
        }
        /* tsthash.cbl:69: MOVE */
        {
          cob_move (&c_4, &f_13);
        }
        /* tsthash.cbl:70: WRITE */
        cob_exception_code = 0;
        {
          cob_move (&f_10, &f_5);
          cob_write (h_HASH_FILE, &f_5, 0, 0);
        }
        if (unlikely(cob_exception_code != 0))
          {
            /* PERFORM standard_error_handler THRU standard_error_handler */
            frame_index++;
            frame_stack[frame_index].perform_through = 1;
            frame_stack[frame_index].return_address = &&l_12;
            goto l_1;
            l_12:
            frame_index--;
          }
      }
    }
  else
  if (unlikely(cob_exception_code != 0))
    {
      /* PERFORM standard_error_handler THRU standard_error_handler */
      frame_index++;
      frame_stack[frame_index].perform_through = 1;
      frame_stack[frame_index].return_address = &&l_13;
      goto l_1;
      l_13:
      frame_index--;
    }
  else
  own_memcpy (b_10, b_9, 32);
  /* tsthash.cbl:71: PERFORM */
  {
    (*(unsigned char *) (b_33)) = 1;
    while (1)
      {
        if (((int)cob_cmp_u8_binary (b_33, 12) >  0))
          break;
        /* PERFORM ADD-ENTRY THRU ADD-ENTRY */
        frame_index++;
        frame_stack[frame_index].perform_through = 6;
        frame_stack[frame_index].return_address = &&l_14;
        goto l_6;
        l_14:
        frame_index--;
        cob_add_u8_binary (b_33, 1);
      }
  }
  if (frame_stack[frame_index].perform_through == 5)
    goto *frame_stack[frame_index].return_address;

  /* ADD-ENTRY: */
  l_6:;
  /* tsthash.cbl:74: CALL */
  {
    {
      int (*func)();
      module.cob_procedure_parameters[0] = &f_14;
      module.cob_procedure_parameters[1] = (f0.size = 7, f0.data = b_15 + 37 * ((*(unsigned char *) (b_33)) - 1), f0.attr = &a_3, &f0);
      module.cob_procedure_parameters[2] = &f_11;
      cob_call_params = 3;
      func = cob_resolve_1 ((const char *)"HASH-CODE");
      (*(int *) (b_1)) = func (b_14, b_15 + 37 * ((*(unsigned char *) (b_33)) - 1), b_10);
    }
  }
  /* tsthash.cbl:75: ADD */
  {
    cob_add (&f_14, &c_5, 2);
  }
  /* tsthash.cbl:76: PERFORM */
  {
    /* PERFORM report-looking THRU report-looking */
    frame_index++;
    frame_stack[frame_index].perform_through = 8;
    frame_stack[frame_index].return_address = &&l_15;
    goto l_8;
    l_15:
    frame_index--;
  }
  /* tsthash.cbl:77: READ */
  cob_exception_code = 0;
  {
    cob_read (h_HASH_FILE, &f_14, 0, 0);
  }
  if (unlikely(cob_exception_code == 0x0506))
    {
      {
        /* tsthash.cbl:79: SET */
        {
          *(b_35) = 51;
        }
      }
    }
  else
  if (unlikely(cob_exception_code != 0))
    {
      /* PERFORM standard_error_handler THRU standard_error_handler */
      frame_index++;
      frame_stack[frame_index].perform_through = 1;
      frame_stack[frame_index].return_address = &&l_16;
      goto l_1;
      l_16:
      frame_index--;
    }
  else
    {
      {
        /* tsthash.cbl:81: SET */
        {
          *(b_35) = 48;
        }
      }
    }
  /* tsthash.cbl:82: MOVE */
  {
    own_memcpy (b_34, b_14, 4);
  }
  /* tsthash.cbl:83: PERFORM */
  {
    while (1)
      {
        if ((!((int)cob_cmp_numdisp (b_35, 1, 0) == 0) || ((int)memcmp (b_15 + 37 * ((*(unsigned char *) (b_33)) - 1), b_9, 7) == 0)))
          break;
        {
          /* tsthash.cbl:84: COMPUTE */
          {
            {
              {
                cob_decimal_set_field (&d0, cob_intr_mod (cob_intr_binop (&f_14, 45, &c_6), &f_11));
                cob_decimal_set_int (&d1, 2);
                cob_decimal_add (&d0, &d1);
                cob_decimal_get_field (&d0, &f_14, 2);
              }
            }
          }
          /* tsthash.cbl:86: IF */
          {
            if (((int)cob_cmp_uint (&f_14, ((unsigned int)COB_BSWAP_32(*(int *)(b_34)))) == 0))
              {
                /* tsthash.cbl:87: SET */
                {
                  *(b_35) = 50;
                }
              }
            else
              {
                /* tsthash.cbl:89: READ */
                cob_exception_code = 0;
                {
                  cob_read (h_HASH_FILE, &f_14, 0, 0);
                }
                if (unlikely(cob_exception_code == 0x0506))
                  {
                    {
                      /* tsthash.cbl:91: SET */
                      {
                        *(b_35) = 51;
                      }
                    }
                  }
                else
                if (unlikely(cob_exception_code != 0))
                  {
                    /* PERFORM standard_error_handler THRU standard_error_handler */
                    frame_index++;
                    frame_stack[frame_index].perform_through = 1;
                    frame_stack[frame_index].return_address = &&l_17;
                    goto l_1;
                    l_17:
                    frame_index--;
                  }
                /* tsthash.cbl:93: PERFORM */
                {
                  /* PERFORM report-looking THRU report-looking */
                  frame_index++;
                  frame_stack[frame_index].perform_through = 8;
                  frame_stack[frame_index].return_address = &&l_18;
                  goto l_8;
                  l_18:
                  frame_index--;
                }
              }
          }
        }
      }
  }
  /* tsthash.cbl:96: IF */
  {
    if (((int)cob_cmp_numdisp (b_35, 1, 3) == 0))
      {
        /* tsthash.cbl:97: MOVE */
        {
          own_memcpy (b_9, b_15 + 37 * ((*(unsigned char *) (b_33)) - 1), 7);
        }
        /* tsthash.cbl:98: MOVE */
        {
          own_memcpy (b_9 + 7, b_15 + 7 + 37 * ((*(unsigned char *) (b_33)) - 1), 20);
        }
        /* tsthash.cbl:99: MOVE */
        {
          cob_move ((f0.size = 10, f0.data = b_15 + 27 + 37 * ((*(unsigned char *) (b_33)) - 1), f0.attr = &a_6, &f0), &f_8);
        }
        /* tsthash.cbl:100: WRITE */
        cob_exception_code = 0;
        {
          cob_write (h_HASH_FILE, &f_5, 0, 0);
        }
        if (unlikely(cob_exception_code != 0))
          {
            /* PERFORM standard_error_handler THRU standard_error_handler */
            frame_index++;
            frame_stack[frame_index].perform_through = 1;
            frame_stack[frame_index].return_address = &&l_19;
            goto l_1;
            l_19:
            frame_index--;
          }
        /* tsthash.cbl:101: DISPLAY */
        {
          cob_new_display (0, 1, 3, &c_7, (f0.size = 7, f0.data = b_15 + 37 * ((*(unsigned char *) (b_33)) - 1), f0.attr = &a_3, &f0), &c_8);
        }
      }
    else
      {
        /* tsthash.cbl:102: IF */
        {
          if (((int)cob_cmp_numdisp (b_35, 1, 2) == 0))
            {
              /* tsthash.cbl:103: DISPLAY */
              {
                cob_new_display (0, 1, 1, &c_9);
              }
            }
          else
            {
              /* tsthash.cbl:105: DISPLAY */
              {
                cob_new_display (0, 1, 1, &c_10);
              }
            }
        }
      }
  }
  if (frame_stack[frame_index].perform_through == 6)
    goto *frame_stack[frame_index].return_address;

  /* ECHO-TABLE: */
  l_7:;
  /* tsthash.cbl:108: DISPLAY */
  {
    cob_new_display (0, 1, 1, &c_11);
  }
  /* tsthash.cbl:109: DISPLAY */
  {
    cob_new_display (0, 1, 1, &c_12);
  }
  /* tsthash.cbl:110: OPEN */
  {
    cob_exception_code = 0;
    {
      cob_open (h_HASH_FILE, 3, 1, 0);
    }
    if (unlikely(cob_exception_code != 0))
      {
        /* PERFORM standard_error_handler THRU standard_error_handler */
        frame_index++;
        frame_stack[frame_index].perform_through = 1;
        frame_stack[frame_index].return_address = &&l_20;
        goto l_1;
        l_20:
        frame_index--;
      }
  }
  /* tsthash.cbl:111: PERFORM */
  {
    cob_setswp_u32_binary (b_14, 2);
    while (1)
      {
        if (((int)cob_cmp_uint (&f_14, ((unsigned int)COB_BSWAP_32(*(int *)(b_10)))) >  0))
          break;
        {
          /* tsthash.cbl:113: READ */
          cob_exception_code = 0;
          {
            cob_read (h_HASH_FILE, &f_14, 0, 0);
          }
          if (unlikely(cob_exception_code == 0x0506))
            {
              {
                /* tsthash.cbl:115: DISPLAY */
                {
                  cob_new_display (0, 1, 1, &c_13);
                }
              }
            }
          else
          if (unlikely(cob_exception_code != 0))
            {
              /* PERFORM standard_error_handler THRU standard_error_handler */
              frame_index++;
              frame_stack[frame_index].perform_through = 1;
              frame_stack[frame_index].return_address = &&l_21;
              goto l_1;
              l_21:
              frame_index--;
            }
          else
            {
              {
                /* tsthash.cbl:117: DISPLAY */
                {
                  cob_new_display (0, 1, 3, &f_6, &c_11, &f_7);
                }
              }
            }
        }
        cob_addswp_u32_binary (b_14, 1);
      }
  }
  /* tsthash.cbl:120: CLOSE */
  {
    cob_exception_code = 0;
    {
      cob_close (h_HASH_FILE, 0, 0);
    }
    if (unlikely(cob_exception_code != 0))
      {
        /* PERFORM standard_error_handler THRU standard_error_handler */
        frame_index++;
        frame_stack[frame_index].perform_through = 1;
        frame_stack[frame_index].return_address = &&l_22;
        goto l_1;
        l_22:
        frame_index--;
      }
  }
  if (frame_stack[frame_index].perform_through == 7)
    goto *frame_stack[frame_index].return_address;

  /* report-looking: */
  l_8:;
  /* tsthash.cbl:123: MOVE */
  {
    cob_move (&f_14, &f_40);
  }
  /* tsthash.cbl:124: DISPLAY */
  {
    cob_new_display (0, 1, 2, &f_40, &c_14);
  }
  if (frame_stack[frame_index].perform_through == 8)
    goto *frame_stack[frame_index].return_address;

  cob_current_module = cob_current_module->next;
  return (*(int *) (b_1));

  /* error handlers */

  /* standard_error_handler: */
  l_1:;
  switch (cob_error_file->last_open_mode)
  {
    default:
    {
      if (!(cob_error_file->flag_select_features & COB_SELECT_FILE_STATUS)) {
      	cob_default_error_handle ();
      	cob_stop_run (1);
      }
      break;
    }
  }
  if (frame_stack[frame_index].perform_through == 1)
    goto *frame_stack[frame_index].return_address;
  cob_fatal_error (COB_FERROR_CODEGEN);

}

/* end function stuff */

