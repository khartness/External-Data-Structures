/* Generated from tsthash.cbl by cobc version 1.0 patch level 0 */

/* PROGRAM-ID : TST-HASH */

static unsigned char b_1[4] __attribute__((aligned));	/* RETURN-CODE */
static unsigned char b_2[4] __attribute__((aligned));	/* SORT-RETURN */
static unsigned char b_3[4] __attribute__((aligned));	/* NUMBER-OF-CALL-PARAMETERS */
static unsigned char b_10[32] __attribute__((aligned));	/* W-HEADER */
static unsigned char b_14[4] __attribute__((aligned));	/* W-RECPOS */
static unsigned char b_15[444] __attribute__((aligned));	/* TEST-DATA */
static unsigned char b_33[1] __attribute__((aligned));	/* W-POS */
static unsigned char b_34[4] __attribute__((aligned));	/* W-START */
static unsigned char b_35[1] __attribute__((aligned));	/* W-STATUS */
static unsigned char b_40[2] __attribute__((aligned));	/* w-show-pos */
static unsigned char b_9[35] __attribute__((aligned));	/* HASH-FILE_record */

/* attributes */
static cob_field_attr a_1	= {36, 2, 0, 0, "Z\001\000\000\0009\001\000\000\000"};
static cob_field_attr a_2	= {17, 9, 0, 32, NULL};
static cob_field_attr a_3	= {33, 0, 0, 0, NULL};
static cob_field_attr a_4	= {1, 0, 0, 0, NULL};
static cob_field_attr a_5	= {16, 1, 0, 0, NULL};
static cob_field_attr a_6	= {16, 10, 2, 0, NULL};
static cob_field_attr a_7	= {17, 10, 2, 32, NULL};

/* fields */
static cob_field f_7	= {20, b_9 + 7, &a_3};	/* HF-NAME */
static cob_field f_6	= {7, b_9, &a_3};	/* HF-ID */
static cob_field f_8	= {8, b_9 + 27, &a_7};	/* HF-BALANCE */
static cob_field f_11	= {4, b_10, &a_2};	/* W-TABLESIZE */
static cob_field f_5	= {35, b_9, &a_4};	/* HASH-REC */
static cob_field f_10	= {32, b_10, &a_4};	/* W-HEADER */
static cob_field f_13	= {24, b_10 + 8, &a_3};	/* W-SIGNATURE */
static cob_field f_9	= {35, b_9, &a_3};	/* HASH-FILE_record */
static cob_field f_14	= {4, b_14, &a_2};	/* W-RECPOS */
static cob_field f_40	= {2, b_40, &a_1};	/* w-show-pos */

/* constants */
static cob_field c_1	= {12, (unsigned char *)"hashfile.rel", &a_3};
static cob_field c_2	= {12, (unsigned char *)"create-table", &a_3};
static cob_field c_3	= {10, (unsigned char *)"get header", &a_3};
static cob_field c_4	= {8, (unsigned char *)"CUSTHASH", &a_3};
static cob_field c_5	= {1, (unsigned char *)"2", &a_5};
static cob_field c_6	= {1, (unsigned char *)"1", &a_5};
static cob_field c_7	= {4, (unsigned char *)"    ", &a_3};
static cob_field c_8	= {7, (unsigned char *)" saved!", &a_3};
static cob_field c_9	= {14, (unsigned char *)"TABLE IS FULL!", &a_3};
static cob_field c_10	= {13, (unsigned char *)"DUPLICATE ID!", &a_3};
static cob_field c_11	= {1, (unsigned char *)" ", &a_3};
static cob_field c_12	= {9, (unsigned char *)"HASH FILE", &a_3};
static cob_field c_13	= {21, (unsigned char *)"       <EMPTY RECORD>", &a_3};
static cob_field c_14	= {5, (unsigned char *)" read", &a_3};


/* cob fields */
cob_field f0;

/* ---------------------------------------------- */

