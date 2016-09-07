%Module BY_LOGS_DMP <main>;
*****************************************************************
*								*
* Copyright 1987 - 2005 by IntraNet, Inc. All rights reserved.	*
*								*
* This Software is confidential and proprietary to IntraNet	*
* and it is protected by U.S. copyright law, other national	*
* copyright laws, and international treaties. The Software may	*
* not be disclosed or reproduced in whole or in part in any	*
* manner to any third party without the express prior written	*
* consent of IntraNet, Inc.					*
*								*
* This Software and its related Documentation are made		*
* available under the terms of the IntraNet Software License	*
* and may not be used, reproduced or disclosed in any manner	*
* except as expressly authorized by the Software License. All	*
* other uses are strictly prohibited.				*
*								*
* This Software and its related Documentation are proprietary	*
* and confidential material of IntraNet, Inc.			*
*								*
*****************************************************************

* Creates a "flat" RMS file of message data for all messages. By default, the
* current period, and all banks, are dumped. However, a period and a single
* bank may be specified. This particular dump program is driven by the contents
* of a list of processing logs, the idea being to select all messages processed
* on a specific day. The logs are:
*
*	the TRN REF_INDEX
*	the accounting logs:	{RISK_, PAYADV_, SECPAYADV}LOG
*	the cancellation logs:	{CANCEL, FTRRMV, ADMRMV}_LOG
*	the delivery logs:	{FED, SEC}OUT_LOG
*
* The MSG_DUMP module converts a single message to a flat record.  Originally
* MSG_DUMP was part of BY_LOGS_DMP; now, BY_LOGS_DMP is a driver for MSG_DUMP.


* REVISION HISTORY
* ----------------
* (Since the revision history from 21-Jul-87 through 25-Mar-91 applies mostly
* to changes in the dumped fields, it now resides in MSG_DUMP.COB).
*
* 26-Mar-91 GH	Split MSG_DUMP part out into its own module, so programs
*		other than BY_LOGS_DMP can get single-message dumps.
*
* 10-Apr-91 GH	Fix newly introduced bug in static dump - was dumping last
*		bank BANK_CNT times.
*
* 31-Aug-92 DB	Create new "accounting" file which has a seperate record for
*		every accounting entry (debit, credit, cross-bank clearance
*		entries, commission accounting, interest/principal, ...).
*
* 23-Sep-92 DB	Match latest PAYADV secondary and tertiary accounting strategy
*		when writing the accounting file.
*
* 8-Oct-92 DB	ASC_DBT/CDT_TRAN_CD's have become DBT/CDT_TRAN_CD_NAMES and 
*		contain the full 8-character tran codes.
*
* 9-JUN-93 FPI	Changed BY_LOGS_DMP_PRINTR_REC to contain currency code and
*		foreign amount for international use.  Liberalized recognition
*		of printer queues in terms of name length.
*
* 12-Nov-93 HP	Call INTRTL_TRAN_TYPE_CHECK to determine NON-accounting tran
*		types.
*
* A. Smith	 3-DEC-1993 10:18:36.47
*		Use QUERY_PERIOD to avoid reading messages which are in the
*		target period and hence would be caught by the REF_INDEX
*		scan. Only messages logged (e.g. on PAYADV_LOG) on the
*		target date but created on a prior date need be dumped.

* P. Labo	 23-MAR-1994 11:18:34.22
*		Qualifiers have been added so that the STATIC file can be
*		run with out without running the rest of the program.  The
*		same is now true for the printer file too.  A qualifier was
*		also added to just run the BIG_BY_LOGS_DUMP, BY_LOGS_DUMP,
*		and ACCOUNTING files.  If no qualifier is specified, then all
*		five files will be generated.  In the future, the plan is
*		generate the ACCOUNTING file via the MFELINK program. This
*		will eliminate the need to run this program for anything but
*		the STATIC and PRINTER files.
*
* G. Johnston	Bring forward following from 3.3:
* A. Smith	27-APR-1994 17:04:06.47
*		Fix bug introduced in 3-DEC change. Query period logic
*		was being used on ref-index, which is a bad idea if
*		a prior day is being dumped.
*
* S. Smith	5-JAN-1995  10:17:00.00
*		Added paramater to Subroutine FILL_ACCTG for Idtype of
*		cdt_account of Msg_credit_set so chips clearing account
*		would not be reported with a "P" idtype. per #3711
*		Also changed  paramaters from CDT_ACCOUNT of Msg_credit_set
*		and DBT_ACCOUNT of Msg_debit_set to Idkey of Cdt_account 
*		Idkey of Dbt_account. These were correct in Msg_dump but not
*		here.
*
* S. Smith	23-MAR-1995 10:14:32
*		Added enhancement, Per#5873, which writes out a file containing
*		one record for each entry on the Msg_history_seq or the 
*		SubHistory_seq. The subroutine Msg_hist_export is called
*		once to open the output file and once to close it. is called 
*		twice from this program, using the routine Call_Via _Pointer,
*		once to open the output file and once to close it. 
*		A additional parameter has been added to the call to Msg_dump	
*		containing the address of the callback routine.
*		This enhancement takes effect if By_logs_dmp is run with the
*		'/HISTORY' qualifier. Note that if no qualifiers are specified,
*		the program defaults to creating all files *except history*:
*		the /HISTORY qualifier must be explicitly specified.
*
* 28-Apr-1995 PG Beer
*		The new IRS 1090 requires the possible accounting against a new
*		Federal reserve account called a TT&L.	Accounting will go against
*		either the TT&L or if it doesn't exist the federal Reserve Account.  
*		TTL values are added in msg_dump_bank_array - similar to frb_...  
*		Logic for checking which account is used is added to 
*		a10_save_bank and a120_acctg
*
* A. Smith	18-MAY-1995 10:42:39.26
*		Accounting file creation is now done by a common subroutine.
*
* K. Griffin	24-MAY-1995
*		Added new parameter (acctg_done_ws) in call to msg_dump.
*		This parameter will not be used by by_logs_dmp.	 Added
*		a check to see if accounting had been performed on the
*		message before the call to acctg_dump.
*
* A. Smith	17-JUN-1995 12:36:51.10
*		Remove CALL_VIA_POINTER where it was extraneous.
*
* S. Smith	7-AUG-1995 16:39:42
*		Brought Forward Per #8573 (with correct name) and spr#8506
*		modified call to acctg_dump to insure thqat accounting file
*		ends up with correct name in apl$reports.
*
* T. Carroll	1-NOV-1995 15:17
*		Corrected the parameter list passed to msg_dump.
*
*  13-Dec-95 PGB spr 12581.  irs 1090.	When tt&l special account doesn't exist
*		 assure that the frb account is put cdt_acctg so that irs 1090s
*		 messages are reported properly.
*
* 27-Dec-95 GH	Add new BY_LOGS_DMP$MTEXT output file if /MTEXT present.
*		This dump contains the message text and a few other fields.
*		It required adding the base currency to the bank array.
*		Dump is currently optimized to only output information of
*		interest to the Trail interface, but it is designed to be
*		easy to expand. Beware: delimits text lines with <29>.
*		Also: remove a misleading comment.
*
*  17-May-96 JRP spr 16815 Implement SPL Cancellation to Exception
*
*  22-JAN-1997	Fred P. Isaacs.
*		Made C20_MTEXT_DMP check new internal reroute ftr set flag.
*		Migrated to 4.2 by John Phelan.
%^
%^ 16-Apr-1997	M. Kuske	SPR27421
%^		Program was trapping because MTEXT file was not being opened
%^		when the DUMP_BYLOGS flag is set to an "N" but was being closed.
%^		MTEXT close logic was moved to within the dump by_logs logic.
*
*  25-APR-1997	A. Joress Ref #24065
*		Added enhancement which writes out to a file
*		(BY_LOGS_DMP$DESTINATION) containing destination information
*		for each entry in the Msg_subhist_seq.	The subroutine
*		DST_DUMP is called once to open the output file and once to
*		close it.  When it is called from MSG_DUMP, it uses the
*		routine Call_Via_Pointer.  An additional parameter has been added
*		to the call to MSG_DUMP containing the address of the callback
*		routine.  This enhancement takes effect if BY_LOGS_DMP is run
*		with the '/DESTINATION' qualifier.  Note, that if no qualifiers
*		are specified, the program defaults to creating all files
*		*except DESTINATION*: the /DESTINATION qualifier must be explicitly
*		specified.
*
* 24-SEP-1997	Tim Spellman #33020
*		Added new BY_LOGS_DMP$RTEXT output file if /RTEXT is present.
*		This dumps the message text as received for all messages.
*		Contrast this with /MTEXT which dumps SWIFT admin text
*		formatted specifically for Trail.
*
* 1-JUN-1998	Zach Los
*		Create variable length RGW set of files. Unix RGW_CVT program
*		requires variable length files to create load files.
*
* 8-JUN-1988	Tim Spellman
*		Added Record_Expired to the end of each of the RGW files.
*
* 3-Jul-1998	Tim Spellman
*		Added support for MESSAGE_SUB table: RGW_BY_LOGS_DMP_SUB.
*
* 9-Jul-1998	Tim Spellman
*		Replaced RGW cloned records with real records and removed the
*		artificial Record_expired fields.
*
* 29-Aug-1998	Zach Los
*		Replaced RGW cloned records with real records and removed the
*		artificial Record_expired fields.
* 31-Aug-1998	Zach Los
*		Replace rgw_message_sub file/record with rgw_message_cr
*		and rgw_message_dr record/files.
* 2-Oct-1998   Penny Godfrey Beer 
*		Modified ref index searches to use 4 character year instead 
*		of 2 character year. Added compose. 
* 8-Oct-1998   Zach Los
*		Periodx in rgw_lil_dmprec should equal now_timestamp
*		that is used in rgw_msg_cr and rgw_msg_dr record.
* 27-Nov-1998	Tim Spellman
*		Added Record_Updated column to each table.
* 11-May-1999	Tim Spellman  Hotrep 54068
*		Blank out RCV_DATE and TIME for RGW if they contain all asterisks.
*
* 19-May-1999	T. Belknap	SPR 54602
*		Instead of looking for trn 1, look for anything greater than
*		(or equal to) 1, in case the first message is not found (as
*		happened)
* 02-June-1999	Steve Aucella
*		Fixed a problem wherein numeric RGW fields were not being set
*		from the ASCII versions in the RGW_LIL_DMPREC, RGW_MSG_CR_DMPREC,
*		and RGW_MSG_DR_DMPREC records.
* 03-June-1999	Zach LOs	SPR 54801
*		Added move statement for msgtype field because move correspondind
*               not working due to field name mismatch.
* 17-Jun-1999	Tim Spellman	55300
*		Add TRAIL file handling.
* 12-July-1999	Steve Aucella
*		Fixed a problem with RGW amount fields getting extra decimal
*		positions when being coverted from ASCII amounts.  See ASCII_AMT_ND.
* 22-July-1999	Steve Aucella
*		Fixed the way amounts are being shipped to RGW in Z808_LIL_SETUP.
*
* 24-Aug-1999	J. Walsh
*		Resynch from VMS V4.5         
*
*		<<<<< Begin RESYNCH SPR List >>>>>
*
*	10-Dec-1997	Tim Spellman
*	Added parameter to DST_DUMP call to pass file name.
*
* 	12-Jan-1998	S. Smith
*	Add paramater to msg_hist_export so all calling programs match 
*	subroutine.
*
*	22-Jan-1998   	A. Joress 38423
*	History file name now being passed to MSG_HIST_EXPORT subroutine.
*
*	25-Jul-1998   	A. Joress
*	Add logic to read an optional file "APL$CONFIG:EOD_TRN_SKIP_LIST.CFG"
*	of TRN's to be excluded from processing.
*
* 	17-Aug-1998	J. Gloor	SPR 46069
*	Put check for successful connect to message history in
*	B20_BIG_DMP and C10_PRT_DMP at beginning of paragraph, *then*
*	connect FTR set.
*	If message history is successfully connected, but there is a
*	a problem connecting the FTR set, the program should trap, since
*	this would mean there was a serious database problem.
*
* 	29-Oct-1998	J. Gloor	SPR 45021
*	Add RGWLINK_FSECT to data definitions.  This allows message
*	dump subroutines to optionally send output to RGWLINK for
*	direct line transfer rather than writing to a file.
*
* 	22-Mar-1999	J. Gloor	SPR 52188
*	Check for successful connections to the FTR, DEBIT, and CREDIT
*	sets before dumping a message.  For bad messages, get the TRN,
*	print out an informative error message, and then either skip
*	the message or trap, depending on whether the logical
*	APL$EOD_SKIP_BAD_MESSAGES is defined or not, respectively.
*
* 	06-Apr-1999	J. Gloor	PER 52056
*	Add new PTEXT dump for the Pegasystems interface.  It is
*	completely analogous to the RTEXT dump, except that all its
*	fields are ASCII and the record layout doesn't include a
*	timestamp.
*
*	29-Jun-1999	J. Gloor	PER 54585
*	Generalize PTEXT dump to include both incoming and outgoing
*	payments text.  If the /ITEXT qualifier is present, the
*	incoming text will be dumped in the PTEXT format.  If the
*	/OTEXT qualifier is present, the primary and secondary payments
*	text will (also) to dumped in the PTEXT format.
*
*		<<<<< End RESYNCH SPR List >>>>>
*
* 09-Nov-1999	T. Spellman	59040
*	All the account fields were being copied into the Dbt_ident field
*	instead of into their own specific fields.
*
* 17-Nov-1999	E. Osterberg	59040
*	Add omitted argument to support Cust_wiretype change.
*
* T. Belknap	15-Dec-1999	SPR 59856
*	Code Health Fixes.
*
* 23-Dec-1999	T. Spellman	PER 54213
*		Added the TEXT_TYPE column to make the table support outgoing 
*		message text.
*
* 29-Dec-1999	T. Spellman	TIR 59135
*		Fixed timestamp in MESSAGE_TEXT table.
*
* 07-Feb-2000	T. Belknap	SPR 60398
*	Fix trail output, date fields misaligned, amount not set.
*
* 27-Apr-2000	R. Beer		SPR 62264
*		Corrected the call to MSG_DUMP.  The problem arose while testing.
*
* 26-May-2000	S. Aucella	SPR 63934
*		Added new fields to big dump rec.
*
* 19-Jul-2000	H. Podany	Spr# 59856
*		Change large-dump-file record size to be in sync with a change made
*		to msg_dump_format_0_rec.ddf.
*
* 17-Aug-2000	T. Spellman	59038
*		Populate the RGW record_expired fields with zero.
*
* T. Spellman	21-Sep-2000	67538
*	Fix errors with TRAIL AMT fields.
*
* R. Beer	6-Dec-2000
*	Provide recovery logic that allows a message to be extracted
*	even when the OPR_ACTION log does not contain a TRN_REF.  The
*	program now connects to the ftr set to get the message number.
*
* R. Beer	13-Apr-2001	73465
*	Expanded the big dump record by adding three fields for IB1_NAME.
*	The record size was updated to reflect the change. 
*
* R. Beer	30-Apr-2001	73894
*	CDT_ACCTG and DBT_ACCTG fields empty in the RGW credit and debit records.
*
* D. Nix	24-May-2001	74403
*	Move amount fields to a pic x(n) redefined with an assumed decimal field
*	with 3 places and use that to move to a field type of amount.
*
* R. Beer	20-Jun-2001	75118
*	EXCH_RATE field in the lil_dmprec was not formatted correctly.
*	The column in the RGW table was empty.  Also fixed trn_dates for 
*	the previous century (1998).
*
* R. Beer	16-Jul-2001	76026
*	Added new exchange rate fields related to CLS.  These fields
*	needed to be reformatted when building the RGW records 
*	(Z8nn paragraphs).  Also changed length of BIG_BY_LOGS_DMP_OUTPUT.
*
* R. Beer	17-Oct-2001	78863
*	Debit and credit tran codes were not moving to the corresponding
*	RGW records because the datanames did not align to the md_dmprec.
*
* R. Beer 	22-Oct-2001	77976
*	Overdraft amount multiplied by 1000 because of decimal point 
*	alignment error.
*
* R. Beer	25-Oct-2001	77276
*	Expanded large dump record length to accomodate addtional fields
*
* C.McKenzie	28-NOV-2001	TRN Expansion
*	Definition of trn_date and trn_num in ent_ftr_set were previously unique 
*	enougth that the references to them could be 'trn_date of ent_ftr_set'
*	and 'trn_num of ent_ftr_set'.  With changes made to the ent_ftr_set for
*	the trn expansion project this is no longer true.  References to them
*	need to be 'trn_date of trn_ref of ent_ftr_set' and
*	'trn_num of trn_ref of ent_ftr_set'.
*
* R. Beer	11-Dec-2001	TRN Expansion
*	Further adjustments to accomodate the expanded Trn_num and Trn_date.
*
* R. Beer	10-Jan-2002	76433 Department expansion
*	Expanded record lengths to reflect larger fields and the new field
*	bank_operation_code.
*
* R. Beer	22-Feb-2002	81230
*	Install the ability to dump related amount sequence data.
*
* J. Phelan	21-Mar-2002	76378
*	Break Msg_related_amount_seq between messages.
*
* R. Beer	15-Apr-2002	SPR 81606
*	Change the call to "TRAN_TYPE_CHECK".  The new subroutine for this
*	is "CUST_TRAN_TYPE".
*
* R. Beer	06-May-2002	77996
*	Added calls to the character substitution routine "REPL_CHARS"
*	to remove control type characters from message text.
*
* R. Beer	24-May-2002	85178
*	Version 1.2 changes
*
* P. G. Beer    12-June-2002  spr 75581
*		Edi qualifier  added to ptext and rtext/rgw 
*
* R. Beer	13-Jun-2002	SPR 83386
*	Added STOP_ADM_LOG to the logs that will be scanned for activity
*	in the current period.  This will pick up admin messages that have 
*	been processed off the OFAC queue.
*
* P. G. Beer	14-Aug-2002	
*	as part of java translation rename msg_dump_bank_array.cob 
* 	to msg_dump_bank_array.cpy
*
* R. Beer	20-Aug-2002	SPR 86393
*	Corrections to text files:  For "O" type the sequence number
*	was multiplied by 10 due to positioning error.  The date field
*	in BY_LOGS_DMP_RTEXT was not treated as a string field.
*
* R. Beer	22-Aug-2002	SPR 88143
*	Program trap in paragraph A70_DUMP_AMT_BAL_LOG when the message
*	has been removed by period erase.  Also program trap in 
*	X15_CONNECT_FOR_TRN when there is no message number in the log
*	(OPRACT) and the message has disappeared.
*
* R. Beer	29-Aug-2002	SPR 88459
*	Additionalal version 1.2 changes.  Adjustment to record lengths.
*
* R. Beer	17-Sep-2002	SPR 88900
*	String field overflow (STROVF) trap when composing into 
*	avail_bal_ascii.  Expand avail_bal and ledger_bal.
*
* D. Nix	10-Oct-2002	SPR 85238
*	Added extract of the new processing rules sequences for RGW tables.
*	This version uses the Prules API.
*
* R. Beer	1-Oct-2002	SPR 87157
*	CONNECT_RETURN_STATUS needed to be set to default value.  It always 
*	indicated failure.
*
* B. Rossin	21-Oct-2002	SPR 89160
*	Correctly populate VALUE_DATE of BY_LOGS_DMP_MTEXT. Previous code
*       was incorrectly trying to write 8 char date to 6 char field.
*
* D. Nix	21-Oct-2002	SPR 90858
*	Map the message level oneof values returned by the processing
*	rules API into its DB level oneof equivalent.
*
* D. Nix	30-Oct-2002	SPR 91145
*	Replace use of the confirmation sequence to obtaining data from
*	the processing rules.
*
* D. Nix	05-Nov-2002	SPR 85238 (spr previously used above)
*	Added some processing options:
*		-trace = Print out some info for debugging purposes.
*		-prule_d*irect = Reads prules directly from the message objects.
*		-default_cnf = Default the number of confirmation copies to 1.
*
* D. Nix	14-Nov-2002
*	Changed the processing option -prule_direct to -prule_api. The default
*	will now be to extract processing rules from the message objects directly.	
*
* R. Beer	18-Dec-2002	SPR 93710
*	Data in the Stop_intercept_flg was lost in the move corresponding
*	statement for the RGW record.  The datanames do not correspond.
*
* D. Nix	15-Jan-2003	SPR 91648
*	Limit selection criteria for PRULE API calls and changed the default
*	extract of prules to be thru the API.
*
* D. Nix	20-Mar-2003	SPR 96568
*	Removed multiple writes of the processing rules parameter execution
*	records when there are multiple values associated with the parameter.
*
* R. Beer	2-May-2003	SPR 98597
*		Add call to new subroutine, NEX_TO_PRINTABLE
*		to replace non-printable characters in memo field.
*		Substituted for call to "REPL_CHARS" for better performance
*
* G. Johnston	8-May-2003	SPR 96674
*		Add timestamp overrride parameter
*
* R. Beer       19-May-2003      PER 72746
*       Add the ability to extract data from destination formatted text
*	sequences and include it in the rtext file.
*
* G. Johnston	29-May-2003	SPR 100345
*		Add move of Dbt tertiary acc id to rgw dr rec.
* G. Johnston	18-Jun-2003	SPR 101420,101300
*		Fix implementation in b30_chk_already_processed
*		was causing a parse trap.
*
* R. Beer	31-Jul-2003	SPR 99207
*		Change format of avail_bal and ledger_bal in the RGW record.
*
*
* G. Johnston	28-Aug-2003	SPR 104159
*		The prules extract was getting dup inserts
*		and empty records on RGW load. Add some status checking
*		and the export of the match values sequence, if evident.
* G. Johnston	07-Nov-2003	SPR 107043
*		b30_chk_processed had disjoint logic on 16 or 12 byte
*		trn check
* G. Johnston	11-Nov-2003	SPR 106642
*		Additional V1.2 fields.
* B. Rossin	12-Nov-2003	SPR 102886
*		Added Dbt_adr_type.Country and Cdt_adr_type.Country to Trail Output
*
* G. Johnston	21-Nov-2003	SPR 107600
*		Fix up bad txt trn message in b30_chk
*
* C. Hansen     05-Jan-2004     SPR 59856
*               made rms-file_records unique. added a00_main_end paragraph
*               for java translation.
*
* G. Johnston	06-Feb-2004	SPR 109325
*	Add cfg driven facility to dump additional logs.
*	See A90_additional_queues.
*
* G. Johnston	09-Feb-2004	SPR 109878
*	Expand dump file size to accomodate postal_code_ovr_flg.
*
* G. Johnston	25-Feb-2004	SPR 110489
*	dbt and cdt adr type info was not exporting to RGW
*
* G. Johnston	01-Mar-2004	TIR 110578
*	Trail records had six byte trn numbers.
*	S/be 8.
*
* G. Johnston	10-Mar-2004	PER 103501
*	Provide for extraction from gen_vstr_index logs, i.e.,
*	expand on 109325.
*
* G. Johnston	06-Apr-2004	SPR 110124
*	Allow export of RBE ctl msgs - tran type CTL.
* G. Johnston	27-May-2004 	SPR 111726
*	Expand by_logs_dmp record by 10 bytes to correct
*	caller id truncation.
* G. Johnston	27-May-2004	SPR 113075
*	Connect cur_frx_index due to execution dependency
*	in prule_match when calling convert_si_amount
*
* G. Johnston	18-Jun-2004
*	Add writing of BY_LOGS_TRN_OUTPUT for
*	audit and recovery purposes.
*
* G. Johnston 	01-Nov-2004
*	Expanded msg_dump_format_0_rec.ddf
*	adds rpr_flg and exc_flg
*
* G. Johnston	03-Nov-2004	SPR 107844
*	Add support for filtering by phase
*	requested on command line.
*
* G. Johnston	18-Nov-2004	SPR 118460
*	Bring forward que_join to subhistory fix from 1.2
*
* G. Johnston	04-Mar-2005	SPR 121228
*		Prules extract via API needed to be changed so that
*		move corresponding did not use a possibly 
*		empty record to populate another record.
* 
* G. Johnston	21-Jun-2005	SPR 124134
*		The linkage of PRULE_MSG_READ_RULE_TEST
*		was changed but the call statement was not updated in this
*		module, causing a segmentation fault.	
*
* T. Carroll	26-Sep-2005			SPR 121202
*		Only emit the BADLOGTXT when it's unable to obtain the
*		tran_ref even after connecting to the message from the log.
*
* J. Walsh	03-Oct-2005
*	Remove id overflow fields as part of V30 cleanup.
*
* T. Carroll	28-Oct-2005			SPR 126919
*		Changed logic dealing with BADLOGTXT again.  We now emit
*		the BADLOGTXT once for each queue name when the tran_ref
*		is missing from the .TXT field of the queue join.
*
* R. Beer	21-Mar-2006			SPR 129642
*		When processing configurable logs double check the
*		message date, in case of defective log text field.
*
* R. Beer	31-Oct-2006	SPR 134038, 131555
*		Change logic for failure conditions related to phases.  Terminate
*		the program with an error message when the phase cannot be determined.
*		Terminate with error code rather than libstop.
*		Change the timing of the skip list check to avoid attempting to 
*		connect to a missing message.
*
* R. Beer	22-Dec-2006
*		Version 3.0 expansion.  New fields added to ftr_set, credit_set, and debit_set.
*
* G. Johnston	31-Jan-2007	Add cust_mask_testkey callout
*				to mask testkey data from RGW.
*
* R. Beer	1-May-2007	SPR 137285
*		Restructure the call to DAT_CONN_MSG so that the message
*		is connected immediately, to protect against failures when
*		the processing rule API is called.
*
* R. Beer	18-Jun-2007			SPR 138780
*		Bilat_account fields not moved to RGW records by 
*		move corresponding because of name differences.
*
* R. Beer	12-Oct-2007			SPR 141301	ICR_003296
*		Adjust format_0 record for new field, Msgtype.
*
* (End of Revision History)
*******************************************************************************

Environment Division.
Input-Output Section.
File-Control.
	Select LARGE_DUMP_FILE	Assign to "BIG_BY_LOGS_DMP_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.
	Select SMALL_DUMP_FILE	Assign to "BY_LOGS_DMP_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.
	Select STAT_DUMP_FILE	Assign to "BY_LOGS_DMP_STATIC"
	       ORGANIZATION IS SEQUENTIAL.
	Select PRT_DUMP_FILE	Assign to "BY_LOGS_DMP_PRINTR"
	       ORGANIZATION IS SEQUENTIAL.
	Select PTEXT_DUMP_FILE	Assign to "BY_LOGS_DMP_PTEXT"
	       ORGANIZATION IS SEQUENTIAL.
	Select MTEXT_DUMP_FILE	Assign to "BY_LOGS_DMP_MTEXT"
	       ORGANIZATION IS SEQUENTIAL.
	Select RTEXT_DUMP_FILE	Assign to "BY_LOGS_DMP_RTEXT"
	       ORGANIZATION IS SEQUENTIAL.
	Select SKIPLIST		Assign to "EOD_TRN_SKIP_LIST.CFG"
	       FILE STATUS IS SKIPLIST_FILE_STAT  
	       ORGANIZATION IS LINE SEQUENTIAL.
	Select TRNLIST		Assign to "BY_LOGS_TRN_OUTPUT"
	       ORGANIZATION IS LINE SEQUENTIAL.

	%^**TRAIL FILES *******************************
	Select TRAIL_DUMP_FILE	Assign to "TRAIL_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.

	%^**RGW FILES **********************************

	Select RGW_SMALL_DUMP_FILE Assign to "RGW_BY_LOGS_DMP_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.
	Select RGW_MESSAGE_CR_FILE Assign to "RGW_MESSAGE_CR_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.
	Select RGW_MESSAGE_DR_FILE Assign to "RGW_MESSAGE_DR_OUTPUT"
	       ORGANIZATION IS SEQUENTIAL.
	Select RGW_RTEXT_DUMP_FILE	Assign to "RGW_BY_LOGS_DMP_RTEXT"
	       ORGANIZATION IS SEQUENTIAL.
	Select RGW_MESSAGE_PR_FILE Assign to "RGW_MESSAGE_PR_OUTPUT"
		organization sequential.
	Select RGW_MESSAGE_PR_MCH_FILE Assign to "RGW_MESSAGE_PR_MCH_OUTPUT"
		organization sequential.
	Select RGW_MESSAGE_PR_PRM_FILE Assign to "RGW_MESSAGE_PR_PRM_OUTPUT"
		organization sequential.
	Select RGW_MESSAGE_PR_PVL_FILE Assign to "RGW_MESSAGE_PR_PVL_OUTPUT"
		organization sequential.

%def		<MSG>		%`SBJ_DD_PATH:MSG_FSECT.DDL`		%end
%def		<RPT_INIT_SUB>	%`SBJ_DD_PATH:RPT_INIT_SUB_FSECT.DDL`	%end
%def		<RGWLINK>	%`SBJ_DD_PATH:RGWLINK_FSECT.DDL`	%end
%def		<ACE>		%`SBJ_DD_PATH:ACE_FSECT.DDL`		%end
%def 		<ENTFTR>	%`SBJ_DD_PATH:ENTFTR_FSECT.DDL`		%end

%def				%^ local fsect

%^ Object Subjects
%^********************************************************************
%^* The trndup_index is used to replace all the diddle processing    *
%^* that existed in by_logs_dmp. We create a temporary scratch index *
%^* for all Trns whose date does not match our processing date.	     *
%^* This eliminates duplicate messages from being processed	     *
%^********************************************************************
TRNDUP_INDEX:		que (%`SBJ_DD_PATH:REF_INDEX.DDF`);

txt16_ws:		vStr(16); %^ for MSG_DUMP_INIT routine
tmp_trn_date:		vstr(8);
tmp_trn_num:		vstr(8);

txt12_ws:		vstr(12); 
tmp_trn_date6:		vstr(6);
tmp_trn_num6:		vstr(6);

Badlogtxt_q:		que(%`SBJ_DD_PATH:GEN_VSTR_INDEX.DDF`);
Badlogtxt_noted_ws:	Boolean;
Badlogtxt_data_ws:	Boolean;

ws_new_trn:		vstr(16);
tmp_date:		date;
b30_chk_ws:		oneof(opr_act_is,amt_bal_log_is,gen_vstr_index_is);
Chk_compose:		Compose(^NOTRAP);
Chk_parse:		Parse(^NOTRAP);

Trn_Date_rgw:		Str(8);
Trn_Num_rgw:		Str(8);

OPRACT:			Que (%`SBJ_DD_PATH:OPR_ACTION_LOG.DDF`);
INDEXA:			Que (%`SBJ_DD_PATH:REF_INDEX.DDF`);
PRINTER:		Que (%`SBJ_DD_PATH:LINE_LOG.DDF`);
AMT_LOG:		Que (%`SBJ_DD_PATH:AMT_LOG.DDF`);
AMT_BAL_LOG:		Que (%`SBJ_DD_PATH:AMT_BAL_LOG.DDF`);
GEN_VSTR_NDX:		Que (%`SBJ_DD_PATH:GEN_VSTR_INDEX.DDF`);
STATUS_WF:		boolean;


%^ Structures to access configuration tables.
Cfg_union_key_ws:	rec (%`SBJ_DD_PATH:CFG_ID_REC.DDF`);
Cfg_item_key_ws:	vstr(25);
Cfg_item_type_ws:	vstr(16);
Cfg_error_msg_ws:	vstr(%`%ACE$_MSG_STR_SIZE`);
Cfg_seq_ordinal_ws:	word;
Cfg_match_key_ws:	vstr(80);
Cfg_data_ws:		vstr(256);
Cfg_status_wf:		boolean;

Lcl_msg_value_seq:	Seq(%`SBJ_DD_PATH:DAT_TEXT_SEQ.DDF`);

%^ Field Subjects
ERRPFX:			Str(20)="%BY_LOGS_DMP-"; %^ for MSG_DUMP_INIT routine
ERRPFX_lgth_ws:		long = <13>;		 %^* WARNING keep this length in sync with ERRPFX
MSG_PERIOD_ws:		time = <0>;	%^ argument to MSG_DUMP call
RETURN_STATUS:		Boolean;   %^ generic return status
RETURN_P_STATUS:	Boolean;   %^ paragraph return status
CONNECT_RETURN_STATUS:	Boolean;   %^ paragraph X15 return status
MSG_UNION_STS:		Boolean;
MSG_FTR_STS:		Boolean;
MSG_DEBIT_STS:		Boolean;
MSG_CREDIT_STS:		Boolean;
BITMAP_ID:		Long;	   %^ from TRN_BITMAP_CREATE
MSG_COUNT:		Long;	   %^ messages dumped from this log
BNK_COUNT:		Long;	   %^ number of bank in bank table
TOT_MSG_COUNT:		Long;	   %^ total messages dumped
ACCTG_REC_COUNT:	Long;	   %^ total accounting records written
TRAIL_REC_COUNT:	Long;	   %^ total TRAIL records written
PTEXT_REC_COUNT:	Long;	   %^ total Pega received-text records written
PTEXT_SEQUENCE_ws:	Long;	   %^ Pega received-text record sequence number
MTEXT_REC_COUNT:	Long;	   %^ total message-text records written
RTEXT_REC_COUNT:	Long;	   %^ total received-text records written
RTEXT_SEQUENCE_ws:	Long;	   %^ received-text record sequence number
BANK_IDX:		Long;	   %^ index to BANK_ARRAY (e.g. current bank)
LIL_DMPREC_Length:	Word;	   %^ size of "little" sub-record
BANK_IDENT:		Str(3);	   %^ scratch for X30_INFORM
Dump_Statlogs_Sts:	BOOLEAN;   %^ flag based on /STATIC qualifier
Dump_Prtlogs_Sts:	BOOLEAN;   %^ flag based on /PRINTER qualifier
Dump_Ants_Sts:		BOOLEAN;   %^ flag based on /INCLUDE_ANTS qualifier
Dump_Bylogs_Sts:	BOOLEAN;   %^ flag based on /BY_LOGS qualifier
Dump_History_Sts:	BOOLEAN;   %^ flag based on /HISTORY qualifier
Dump_Itext_Sts:		BOOLEAN;   %^ flag based on /ITEXT qualifier
Dump_Otext_Sts:		BOOLEAN;   %^ flag based on /OTEXT qualifier
Dump_Mtext_Sts:		BOOLEAN;   %^ flag based on /MTEXT qualifier
Dump_Ptext_Sts:		BOOLEAN;   %^ flag based on /PTEXT qualifier
Dump_Rtext_Sts:		BOOLEAN;   %^ flag based on /RTEXT qualifier
Dump_Dtext_Sts:		BOOLEAN;   %^ flag based on /DTEXT qualifier
Dump_Trail_Sts:		BOOLEAN;   %^ flag based on -trail qualifier
Dump_Dest_Sts:		BOOLEAN;   %^ flag based on /DESTINATION qualifier
Dump_Edi_sts:		BOOLEAN;   %^ flag based on /EDI qualifier
Already_Process_Sts:	BOOLEAN;   %^ flag if a message has already 
				   %^ been processed through the REF_INDEX
Rgw_Sts:		BOOLEAN;   %^ flag to create RGW files
Replace_Char_Sts:	BOOLEAN;   %^ flag to indicate that character 
				   %^ replacement is required
Accounting_Flag:	BOOLEAN;   %^ Success for accounting tran types
Balance_Check_Flag:	BOOLEAN;   %^ Success for balance checking tran types
TRADE_FLAG:		Boolean;   %^ Success for Trade tran types
Prule_direct_sts:	BOOLEAN;   %^ Force reading of prules directly out of the message objects.
Trace_sts:		BOOLEAN;   %^ Print information to the screen for debugging purposes.
rpt_init_status:	Boolean;   %^ Success/Failure on return from rpt_init call

IDX1_WS:		word;
IDX2_WS:		word;

CDT_BANK_IDX:		Long;	%^ Like BANK_INDEX except for CDT party's bank
CDT_BANK_ID:		Str(3);
SEC_BANK_IDX:		Long;	%^ BANK_IDX or CDT_BANK_IDX for secondary acctg
SEC_BANK_ID:		Str(3);
SEC_OTHER_PARTY:	Oneof(	%`SBJ_DD_PATH:ACC_REASON_ONEOF.DDF`);
IS_A_PRINTER:		Vstr(15) ;
PRT_START:		Word ;

temp_vstr_ws:		vstr(80);
%^*****ACE routine arguments*****************************************
Acctg_file_name_ws:	Vstr(200) = "BY_LOGS_DMP_ACCTG.TMP";
return_argument_ws:	Vstr(2048);
Log_acctg_file_name_ws: Vstr(75);
Ws_account:		vstr(24);

%^ The ABA route-transit number of the IRS TT&L tax deposit account:
ws_irs_account_no: str = "091036164";
Acctg_done_ws:		str(1);
Acctg_status_ws:	str(1);	    %^ O=open file, W=write record, C=closefile
File_name_ws:		vstr(75);
Big_dmprec_len_ws:	long;
Bnk_chk_ws:		str(3);
Dest_seq_ws:		long = <-1>;
Dest_ordinal_ws:	long = <-1>;


IDREC_ws:		Rec(
  IDtype_ws:		Str(1);
  IDslash_ws:		Str(1) = "/";
  ID_ws:		Str(24);
  Overflow_ws:		Str(40); );
Include_bank_ws:	boolean;

%^ Parse / Compose
Parse:			Parse;
compose:		compose;
Timestamp_parse_ws:	Parse (^NOTRAP);

%^ Subjects for parsing and composing message text.
input_year:		Str	(2);
output_century:		str	(2);
yyyymmdd_DATE:		Str	(8);
work_DATE:		date;
MTEXT_DATE:		Str	(6);
MTEXT_CUR:		Str	(3);
MTEXT_AMT:		amount;
MTEXT_BUF:		Vstr(2000);
MtParse:		Parse (^NoTrap, ^Line_Skip);
MtCompose:		Compose (^NoTrap);
qprefix_vstr_ws:	vstr(80);
qsuffix_vstr_ws:	vstr(80);
temp_vstr20_ws:		vstr(20);
trn_key_ws: rec(
  trn_date_ws:		str(8);
  trn_num_ws:		str(8); );

end_ref_index_ws:	str(1) = "N"; %^* set when we hit a date break on the ref_index
rec_length_ws:		Long;	      %^* return record length from MSG_DMP, not used in this program

Period_str:		Str(8);
Now_timestamp_ws:	Time;
Now_timestamp_str:	Str(20);

X_timestamp_ws:		Time;
X_timestamp_str:	Str(20);

User_time_wrk:	Rec(Dd:    str(2);
		    Mon:   str(3);
		    Yyyy:  str(4);
                    Hh:    str(2);
		    Mn:    str(2);
		    Ss:    str(2););

User_stamp_ws:		Boolean;
User_time_ws:		Time;
User_timebuf_ws:	Vstr(80);

Skip_trn_seq:		Seq     (%`SBJ_DD_PATH:SKIP_TRN_SEQ.DDF`);
Skip_trn_rec:          	Vstr(132);
Skiplist_file_status:  	Boolean;
Skip_bad_msg_logical:	Str(125);
Skip_bad_msg_str:	Vstr(80);	%^ Non-empty: skip corrupted messages
					%^ Empty: trap on corrupted messages
Skipping_str:		Str(11);	%^ Will be " - skipping" if corrupted
					%^ messages should be skipped; emtpy
					%^ otherwise.  Used for display.

Arg_Number:		LONG;		%^ used in call, number of arguments

repl_chars_in_str:	vstr(80);

Long1:			long;	%^ temporary long to compute edi data length
Vstr1:                  vstr(80); %^ temporary vstr. 
vstr2:			vstr(80); %^ another vstr just like the first.
Edi_type_ws:            Str(4);  %^ Edi Data type 

prule_seq_num_ws:	Long;
values_seq_num_ws:	Long;
Prev_prm_name_ws:	vstr(40);

Getpr_level_asked:	oneof(%` SBJ_DD_PATH:PRULE_MSGLEVEL_ONEOF.DDF`);
Getpr_source_asked:	oneof(%` SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
Getpr_ordinal:		Long;
Getpr_level_found:	oneof(%` SBJ_DD_PATH:PRULE_MSGLEVEL_ONEOF.DDF`);
Getpr_source_found:	oneof(%` SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
Getpr_rule_type:	vstr(80);
Getpr_rule_subtype:	vstr(80);
Getpr_rule_source_id:	vstr(80);
Getpr_rule_descript:	vstr(80);
Getpr_rule_name:	vstr(80);
Getpr_on_datetime:	time;
Getpr_off_datetime:  	time;
Getpr_rtn_cd:		Boolean;

Getex_prm_name:		vstr(40);
Getex_pr_level:		oneof(%` SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
Getex_pr_source:	oneof(%` SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
Getex_prm_edit_type:	oneof(%` SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
Getex_prm_values_remain: long;
Getex_prm_value:	Str(80);
Getex_pr_ret_stat:	Boolean;

prtest_name:		vstr(80);
prtest_value:		vstr(40);
prtest_cond_oneof:	oneof(%` SBJ_DD_PATH:DAT_LOG_COND_ONEOF.DDF`);	
prtest_op_oneof:	oneof(%` SBJ_DD_PATH:PR_LOGICAL_ONEOF.DDF`);	
prtest_multi_values:	long;

match_val_seq_conn:	Boolean;
match_value_seq:	Seq (%`SBJ_DD_PATH:DAT_TEXT_SEQ.DDF`);


more_ex_parms:		boolean;
more_tests:		boolean;
prule_conn_sts:		boolean;

Start_new_tran_ws:	boolean;
Use_timestmp_ws:	Str(20);
chk_subhist_conn_ws:	boolean;

Cust_mask_key_func_ws:	Str(3);
%End

%File
FD  SMALL_DUMP_FILE record 1196 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD1.
01  RMS-FILE-RECORD1 Pic X(1196).
01  LIL_DMPREC	%rec(SBJ_DD_PATH:BY_LOGS_DMP_REC.DDF);

FD  LARGE_DUMP_FILE record 9477 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD2.
01  RMS-FILE-RECORD2 Pic X(9477).
01  BIG_DMPREC	%rec(SBJ_DD_PATH:MSG_DUMP_FORMAT_0_REC.DDF);

FD  STAT_DUMP_FILE record 50 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD3.
01  RMS-FILE-RECORD3 Pic X(50).
01  STAT_DMPREC %rec(SBJ_DD_PATH:BY_LOGS_DMP_STATIC_REC.DDF);

FD  PRT_DUMP_FILE record 190 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD4.
01  RMS-FILE-RECORD4 Pic X(190).
01  PRT_DMPREC	%rec(SBJ_DD_PATH:BY_LOGS_DMP_PRINTR_REC.DDF);

FD  MTEXT_DUMP_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on MTEXT_REC_LEN
    data record is RMS-FILE-RECORD5.
01  RMS-FILE-RECORD5 Pic X(16384).
01  MTEXT_DMPREC	%rec(SBJ_DD_PATH:BY_LOGS_DMP_MTEXT_REC.DDF);

FD  PTEXT_DUMP_FILE record 124 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD6.
01  RMS-FILE-RECORD6 Pic X(124).
01  PTEXT_DMPREC %rec(SBJ_DD_PATH:BY_LOGS_DMP_PTEXT_REC.DDF);

FD  RTEXT_DUMP_FILE record 133 characters
    RECORDING MODE F
    data record is RMS-FILE-RECORD7.
01  RMS-FILE-RECORD7 Pic X(133).
01  RTEXT_DMPREC %rec(SBJ_DD_PATH:BY_LOGS_DMP_RTEXT_REC.DDF);

%^** TRAIL file definitions ****

FD  TRAIL_DUMP_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on TRAIL_REC_LEN
    data record is RMS-FILE-RECORD8.
01  RMS-FILE-RECORD8 Pic X(16384).
01  TRAIL_DMPREC	%rec(SBJ_DD_PATH:TRAIL_TRANSACTIONS_REC.DDF);

%^** RGW file definitions ****

FD  RGW_SMALL_DUMP_FILE
%^**  record 1094 characters
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_LIL_REC_LEN
    data record is RMS-FILE-RECORD9.

01  RMS-FILE-RECORD9 Pic X(16384).
01  RGW_LIL_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_REC.DDF);

%^FD  RGW_SUB_DUMP_FILE
%^** record 5009 characters
%^    RECORDING MODE V
%^    record varying size from 1 to 16384 characters
%^    depending on RGW_SUB_REC_LEN
%^    data record is RMS-FILE-RECORD10.
%^01  RMS-FILE-RECORD10 Pic X(16384).
%^01  RGW_SUB_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_SUB_REC.DDF);

FD  RGW_MESSAGE_CR_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_CR_REC_LEN
    data record is RMS-FILE-RECORD11.
01  RMS-FILE-RECORD11 Pic X(16384).
01  RGW_MSG_CR_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_CR_REC.DDF);

FD  RGW_MESSAGE_DR_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_DR_REC_LEN
    data record is RMS-FILE-RECORD12.
01  RMS-FILE-RECORD12 Pic X(16384).
01  RGW_MSG_DR_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_DR_REC.DDF);

FD  RGW_RTEXT_DUMP_FILE
%^*** record 118 characters
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_RTEXT_REC_LEN
    data record is RMS-FILE-RECORD13.
01  RMS-FILE-RECORD13 Pic X(16384).
01  RGW_RTEXT_DMPREC %rec(SBJ_DD_PATH:RGW_MESSAGE_TEXT_REC.DDF);

FD SKIPLIST
    RECORDING MODE V
    record varying size from 1 to 132 characters
    depending on SKIPLIST_REC_LEN
    data record is SKIPLIST-REC.
01 SKIPLIST-REC         Pic X(132).

%^ potentially this file can "become" a skip list file
%^ this file is a list of all successfully written out trns.
FD TRNLIST
    RECORDING MODE V
    record varying size from 1 to 132 characters
    depending on TRNLIST_REC_LEN
    data record is TRNLIST-REC.
01 TRNLIST-REC         Pic X(132).

FD  RGW_MESSAGE_PR_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_PR_REC_LEN
    data record is RMS-FILE-RECORD14.
01  RMS-FILE-RECORD14 Pic X(16384).
01  RGW_MSG_PR_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_PR_REC.DDF);

FD  RGW_MESSAGE_PR_MCH_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_PR_MCH_REC_LEN
    data record is RMS-FILE-RECORD15.
01  RMS-FILE-RECORD15 Pic X(16384).
01  RGW_MSG_PR_MCH_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_PR_MCH_REC.DDF);

FD  RGW_MESSAGE_PR_PRM_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_PR_PRM_REC_LEN
    data record is RMS-FILE-RECORD16.
01  RMS-FILE-RECORD16 Pic X(16384).
01  RGW_MSG_PR_PRM_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_PR_PRM_REC.DDF);

FD  RGW_MESSAGE_PR_PVL_FILE
    RECORDING MODE V
    record varying size from 1 to 16384 characters
    depending on RGW_MESSAGE_PR_PVL_REC_LEN
    data record is RMS-FILE-RECORD17.
01  RMS-FILE-RECORD17 Pic X(16384).
01  RGW_MSG_PR_PVL_DMPREC  %rec(SBJ_DD_PATH:RGW_MESSAGE_PR_PVL_REC.DDF);

%Work

* The following BANK_ARRAY is shared between MSG_DUMP_INIT, MSG_DUMP, and
* BY_LOGS_DMP.	It is built by MSG_DUMP_INIT, and read by the other two.
* The "BANK_ARRAY_MAX" field contains the "BANK_INFO" record "occurs" count;
* MSG_DUMP_INIT sets the "BANK_CNT" field to the actual number of records.

Copy "msg_dump_bank_array.cpy".

01 Long_string		pic 9999999999 usage display.
01 ASCII_AMT_ND		pic 9(18) usage display.
01 ASCII_AMT		pic 9(15)v9(3) usage display.
01 STR18_AMT redefines ASCII_AMT.
   02 filler		pic x(18).
01 ASCII_EXCH		pic 9(7)v9(11) usage display.
01 STR18_EXCH redefines ASCII_EXCH.
   02 filler		pic x(18).
01 CONVERT_LONG_NUM	pic 9(9) usage display.
01 CONVERT_LONG_STR redefines CONVERT_LONG_NUM.
   02 filler		pic x(9).

01  DST_ORDINAL_WS		pic X(6).
01  DST_ORDINAL_NUM		redefines DST_ORDINAL_WS	pic 9(6).


* Externals
01 SKIPLIST_FILE_STAT  IS GLOBAL EXTERNAL   pic XX.

* These are needed for Msg_hist_export subroutine.
01 Mode_ws		pic x(4).
01 Dmp_rtn_addr		pointer.
01 Dst_dump_name_ws     PIC X(75) value "BY_LOGS_DMP_DESTINATION.DAT".
01 Dump_name_ls		pic x(75) value "BY_LOGS_DMP_HISTORY".
01 Dst_rtn_addr		pointer.

* These are needed for DST_DUMP subroutine.
01 Dst_mode_ws		pic x(4).
01  TRAIL_REC_LEN		pic 9999 comp-5 value zeroes.
01  MTEXT_REC_LEN		pic 9999 comp-5 value zeroes.
01  RGW_RTEXT_REC_LEN		pic 9999 comp-5 value zeroes.
01  RGW_LIL_REC_LEN		pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_DR_REC_LEN	pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_CR_REC_LEN	pic 9999 comp-5 value zeroes.
01  SKIPLIST_REC_LEN 		pic 9999 comp-5 value zeroes.
01  TRNLIST_REC_LEN 		pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_PR_REC_LEN	pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_PR_MCH_REC_LEN	pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_PR_PRM_REC_LEN	pic 9999 comp-5 value zeroes.
01  RGW_MESSAGE_PR_PVL_REC_LEN	pic 9999 comp-5 value zeroes.

* These are needed for RELATED_AMT_DUMP subroutine.
01  Rel_amt_mode_ws		pic x(1).

%Linkage

01 abort_ls			%long;


%Procedure returning abort_ls.

A00_MAIN.

*  Assume success
	Move 0 to abort_ls.

	Perform A10_SBJ_INIT  thru A10_SBJ_INIT_end.
	Perform B25_BUILD_TRN_SKIP_LIST thru B25_BUILD_TRN_SKIP_LIST_end.
	Perform A20_STAT_DUMP thru A20_STAT_DUMP_end.
	Perform A30_PRT_DUMP  thru A30_PRT_DUMP_end.

	If (Failure_Is IN Dump_Bylogs_Sts)
	then
	    Go to A00_MAIN_CLEANUP
	END-IF.

	Open Output LARGE_DUMP_FILE.
	Open Output SMALL_DUMP_FILE.
	Open Output TRNLIST.

	If (Success_Is IN Rgw_Sts)
	then 
	   Open Output RGW_MESSAGE_CR_FILE
	   Open Output RGW_MESSAGE_DR_FILE
%^**	   Open Output RGW_SUB_DUMP_FILE
	   Open Output RGW_SMALL_DUMP_FILE
	   Open Output RGW_MESSAGE_PR_FILE
	   Open Output RGW_MESSAGE_PR_MCH_FILE
	   Open Output RGW_MESSAGE_PR_PRM_FILE
	   Open Output RGW_MESSAGE_PR_PVL_FILE
	End-if.

	If ace_arg_count_ls > 1
	Then
	    Call "ACE_ARG_FIND" using
			by content "-f*ile:",
			by content    "C",
			by value     100
			by reference return_argument_ws,
			by reference return_argument_ws_length,
			by reference OMITTED,
			by reference OMITTED,
			returning return_status
	    if success_is in return_status
	    then
		move spaces	     to Acctg_file_name_ws
		move return_argument_ws(1:return_argument_ws_length)
				     to Acctg_file_name_ws
		%beg Acctg_file_name_ws change; %end
	    end-if
	end-if.

	move length of Big_dmprec to Big_dmprec_len_ws.
	Move "O" to Acctg_status_ws.		%^ Open accounting file
	call "ACCTG_DUMP" using
	  by reference Acctg_status_ws
	  by reference Acctg_file_name_ws
	  by reference Big_dmprec
	  by reference Big_dmprec_len_ws
	  by reference Acctg_rec_count
	  by reference Use_timestmp_ws.

* If the '/TRAIL' qualifier was present then open that output file.
	IF (Success_Is IN Dump_Trail_Sts)
	then
	    Open Output TRAIL_DUMP_FILE
	end-If.


* If the '/ITEXT' or '/OTEXT' qualifier was present then open the PTEXT output file.
	If (Success_Is IN Dump_Itext_Sts)
	  or (Success_Is IN Dump_Otext_Sts)
	then
	    Open Output PTEXT_DUMP_FILE
	end-If.

* If the '/MTEXT' qualifier was present then open that output file.
	If (Success_Is IN Dump_Mtext_Sts)
	then
	    Open Output MTEXT_DUMP_FILE
	end-If.

* If the '/RTEXT' qualifier was present then open that output file.
	If (Success_Is IN Dump_Rtext_Sts)
	then
	    Open Output RTEXT_DUMP_FILE
	    If (Success_Is IN Rgw_Sts)
	    then 
		Open Output RGW_RTEXT_DUMP_FILE
	    End-if
	end-If.

*   if the '/HISTORY' qualifier was present then we open the output file here 
*   for the subroutine Msg_hist_export to write into. Per #5873

	IF (Success_Is IN Dump_History_Sts)
	then
	    Move "OPEN" to Mode_ws
	    Call "MSG_HIST_EXPORT" using
		By Reference	MODE_WS,
		By Reference	Dump_name_ls,
		By Reference	Start_New_Tran_ws,
		By Reference	Use_timestmp_ws
	End-if.

*   if the '/DESTINATION' qualifier was present then we open the output file here.
*   for the subroutine DST_DUMP to write into.

	If (Success_Is IN Dump_Dest_Sts)
	then
	    Move "OPEN" to Dst_mode_ws
	    Call "DST_DUMP" using
		By Reference DST_MODE_WS,
		By Reference DST_DUMP_NAME_WS,
		By Reference Use_timestmp_ws
	End-if.

*	If the -rgw argument was present open the file for related amount data.

	If (Success_is in rgw_sts)
	then
	    Move "O" to Rel_amt_mode_ws
	    Call "RELATED_AMT_DUMP" using 
		By Reference Rel_amt_mode_ws

	End-if.


* Scan ref-index first, thereby dumping all messages entered today.
	Move 0		       to MSG_COUNT.
	Perform A40_DUMP_INDEX thru A40_DUMP_INDEX_end.
	Perform X30_INFORM     thru X30_INFORM_end.
* tell that we are dumping logs
	call "NEX_CREATE_AND_BROADCAST_MSG" using 
		  by content   Z"BY_LOGS_DMP$_DUMPLOGS"
		  by value -1
		  %ace_msg_arg_list(trn_key_ws.trn_date_ws);.

* Dump all other messages processed today, by scanning certain other logs.
* Find rest of logs by scanning QUE_ROOT_NDX to find them.
       %Beg QUE_ROOT_INDEX ^First; %End.
	Perform until failure_is in QUE_ROOT_INDEX_Status
	    set failure_is in chk_subhist_conn_ws to true
	    Move IDBANK of Q_KEY of QUE_ROOT_INDEX to bnk_chk_ws
	    Perform E50_BNK_CHECK thru
		    E50_BNK_CHECK_END
	    If success_is in include_bank_ws
%^	    If BNK_STR = spaces or IDBANK of Q_KEY of QUE_ROOT_INDEX
	    then
		Move 0 to MSG_COUNT
		Evaluate TRUE
		when IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "CANCEL_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "CANEXC_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "FTRRMV_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "ADMRMV_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "RISK_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "PAYADV_LOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "SECPAYADVLOG"
		  or IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "STOP_ADM_LOG"
		     Perform A50_DUMP_OPRACT  thru A50_DUMP_OPRACT_end
		when IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "FEDOUT_LOG"
		when IDNAME of Q_KEY of QUE_ROOT_INDEX	    = "SECOUT_LOG"
		     Set success_is in chk_subhist_conn_ws to true
		     Perform A70_DUMP_AMT_BAL_LOG thru A70_DUMP_AMT_BAL_LOG_end
%^ check config for other queues not hard coded here
%^ 109325,103501
		when Q_TYPE of QUE_ROOT_INDEX	    = "OPR_ACTION_LOG"
		when Q_TYPE of QUE_ROOT_INDEX	    = "AMT_BAL_LOG"
		when Q_TYPE of QUE_ROOT_INDEX	    = "GEN_VSTR_INDEX"
		     Set success_is in chk_subhist_conn_ws to true
		     Perform A90_ADDITIONAL_QUEUES thru
			     A90_ADDITIONAL_QUEUES_END
		end-Evaluate

		Perform X30_INFORM thru X30_INFORM_end
	    end-If
	   %Beg QUE_ROOT_INDEX ^Next; %End
	end-Perform.

	Close LARGE_DUMP_FILE.
	Close SMALL_DUMP_FILE.
	Close TRNLIST.
	If (Success_Is IN Rgw_Sts)
	then
	   Close RGW_MESSAGE_CR_FILE
	   Close RGW_MESSAGE_DR_FILE
	   Close RGW_SMALL_DUMP_FILE
           Close RGW_MESSAGE_PR_FILE
           Close RGW_MESSAGE_PR_MCH_FILE
           Close RGW_MESSAGE_PR_PRM_FILE
           Close RGW_MESSAGE_PR_PVL_FILE
	End-if.


%^ Call accounting subroutine to CLOSE accounting file
	Move "C" to Acctg_status_ws.
	call "ACCTG_DUMP" using
			by reference Acctg_status_ws
			by reference Acctg_file_name_ws
			by reference Big_dmprec
			by reference Big_dmprec_len_ws
			by reference Acctg_rec_count
			by reference Use_timestmp_ws.

*   If the Dump_history flag is set then we need to close up the history
*   file created by the callback routine Msg_hist_export. Per #5873

	IF (Success_Is IN Dump_History_Sts)
	then
	    Move "CLOS" to Mode_ws
	    Call "MSG_HIST_EXPORT" using
		By Reference MODE_WS,
		By Reference Dump_name_ls,
  		By Reference Start_New_Tran_ws,
		By Reference Use_timestmp_ws
	End-if.

*   if the '/DESTINATION' qualifier was present then we close the output file here.
*   file created by the callback routine DST_DUMP.

	If (Success_Is IN Dump_Dest_Sts)
	then
	    Move "CLOS" to Dst_mode_ws
	    Call "DST_DUMP" using
		By Reference	DST_MODE_WS,
		By Reference    DST_DUMP_NAME_WS,
		By Reference	Use_timestmp_ws
	End-if.


*	If the -rgw argument was present close the file for related amount data.

	If (Success_is in rgw_sts)
	then
	    Move "C" to Rel_amt_mode_ws
	    Call "RELATED_AMT_DUMP" using 
		By Reference Rel_amt_mode_ws

	End-if.


	%^*********************************************************************
	%^* BY_LOGS_DMP$_COUNTMSG     /I Total number of messages dumped: ${1}.
	%^*********************************************************************
	call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTMSG"
		      by value -1
		      %ace_msg_arg_list(TOT_MSG_COUNT);.

	%^*********************************************************************
	%^* BY_LOGS_DMP$_COUNTACCT    /I Total number of accounting records dumped: ${1}.
	%^*********************************************************************
	call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTACCT"
		      by value -1
		      %ace_msg_arg_list(ACCTG_REC_COUNT);.

* If dumped TRAIL, close file and announce.

	If (Success_Is IN Dump_Trail_Sts)
	then
	    Close TRAIL_DUMP_FILE
	    %^*********************************************************************
	    %^* BY_LOGS_DMP$_COUNTMTXT	  /I Total number of TRAIL records dumped: ${1}.
	    %^*********************************************************************
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTRAIL"
		      by value -1
		      %ace_msg_arg_list(TRAIL_REC_COUNT);
	end-If.

* If dumped incoming or outgoing payments text, close file and announce.

	If ((Success_Is IN Dump_Itext_Sts)
	  or (Success_Is IN Dump_Otext_Sts))
	then
	    Close PTEXT_DUMP_FILE
%^*********************************************************************
%^* BY_LOGS_DMP$_COUNTMTXT /I Total number of message text records dumped: ${1}.
%^*********************************************************************
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTMTXT"
		      by value -1
		      %ace_msg_arg_list(PTEXT_REC_COUNT);
	End-if.

* If dumped message text, close file and announce.

	If (Success_Is IN Dump_Mtext_Sts)
	then
	    Close MTEXT_DUMP_FILE
%^*********************************************************************
%^* BY_LOGS_DMP$_COUNTMTXT	  /I Total number of message text records dumped: ${1}.
%^*********************************************************************
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTMTXT"
		      by value -1
		      %ace_msg_arg_list(MTEXT_REC_COUNT);
	end-If.


* If dumped received text, close file and announce.

	IF (Success_Is IN Dump_Rtext_Sts)
	then
	    Close RTEXT_DUMP_FILE
%^*****************************************************************************
%^* BY_LOGS_DMP$_COUNTRTXT	  /I Total number of received text records dumped: ${1}.
%^*****************************************************************************
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      by content   Z"BY_LOGS_DMP$_COUNTRTXT"
		      by value -1
		      %ace_msg_arg_list(RTEXT_REC_COUNT);
	    If (Success_Is IN Rgw_Sts)
	    then
	       Close RGW_RTEXT_DUMP_FILE
	    End-if
	end-If.

* Exit

A00_MAIN_CLEANUP.

	%beg 
	break: trndup_index; 
	break: Badlogtxt_q;
	%end.

a00_main_end.
	%exit program;.


A10_SBJ_INIT.

	compute TRAIL_REC_LEN	   = function LENGTH(TRAIL_DMPREC).
	compute MTEXT_REC_LEN      = function LENGTH(MTEXT_DMPREC).
	compute RGW_LIL_REC_LEN	   = function LENGTH(RGW_LIL_DMPREC).
	compute RGW_RTEXT_REC_LEN  = function LENGTH(RGW_RTEXT_DMPREC).
	compute RGW_MESSAGE_CR_REC_LEN
			  = function LENGTH(RGW_MSG_CR_DMPREC).
	compute RGW_MESSAGE_DR_REC_LEN
			  = function LENGTH(RGW_MSG_DR_DMPREC).
	compute RGW_MESSAGE_PR_REC_LEN
			  = function LENGTH(RGW_MSG_PR_DMPREC).
	compute RGW_MESSAGE_PR_MCH_REC_LEN
			  = function LENGTH(RGW_MSG_PR_MCH_DMPREC).
	compute RGW_MESSAGE_PR_PRM_REC_LEN
			  = function LENGTH(RGW_MSG_PR_PRM_DMPREC).
	compute RGW_MESSAGE_PR_PVL_REC_LEN
			  = function LENGTH(RGW_MSG_PR_PVL_DMPREC).
	compute SKIPLIST_REC_LEN   = function LENGTH(SKIPLIST_REC).
	compute TRNLIST_REC_LEN   = function LENGTH(SKIPLIST_REC).

* Connect to top-level objects. Set utility subjects read-only.
       %Beg
	  MSG_UNION		(NoMod);
	  MSG_FTR_SET		(NoMod);
	  MSG_HISTORY_SEQ	(noMod,NoTrap);
	  MSG_DEBIT_SEQ		(nomod);
	  MSG_DEBIT_SET		(NoMod);
	  MSG_CREDIT_SEQ	(Nomod);
	  MSG_CREDIT_SET	(NoMod);
	  MSG_TEXT_SEQ		(NoMod,NoTrap);
	  MSG_CR_PAY_SEQ	(NoMod,NoTrap);
	  MSG_CR_SEC_SEQ	(NoMod,NoTrap);
	  OPRACT		(Read_Only);
	  INDEXA		(Read_Only);
	  PRINTER		(Read_Only);
	  AMT_LOG		(Read_Only);
	  GEN_VSTR_NDX		(Read_Only);
	  Scratch_domain ALLOC: trndup_index(insert);
       %End.

	call "DAT_CONN_ROOT".	%^** added call to dat_conn_root

	Set OTHER_PARTY in	SEC_OTHER_PARTY to true.

* Call RPT_INIT to parse standard /BANK and /DATE command line qualifiers.
	Call "RPT_INIT" returning rpt_init_status.

	If Failure_is in rpt_init_status
		Move 1 to abort_ls
		%EXIT PROGRAM;
	End-if

	%^** rpt_date should have been set by rpt_init
	%beg
	    compose ^out(trn_key_ws),
		Rpt_Date.yyyymmdd, "00000001"/; 
	%end.

* Indicates whether to print information to the screen that could be used
* for debugging purposes.
*
	Call "ACE_ARG_FIND" using
	      by content "-trace*:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Trace_sts.

* Note whether /STATIC given (wants output of BY_LOGS_DMP_STATIC.DAT file).

	%^* This should be a switch only, no data required with switch
	%^* See if STATIC option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-stat*ic:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Statlogs_Sts.

* Note whether /PRINTER given (wants output of BY_LOGS_DMP_PRINTR.DAT file).

	%^*** See if PRINTER option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-print*er:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Prtlogs_Sts.

* Note whether /INCLUDE_ANTS given (wants to include Anticipations in dump file).
	%^*** See if INCLUDE_ANT option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-include_a*nts:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Dump_Ants_Sts.

* Note whether /BY_LOGS given - wants output of BIG_BY_LOGS_DMP_OUTPUT.DAT file
*				wants output of BY_LOGS_DMP_OUTPUT.DAT file
*				wants output of BY_LOGS_DMP_ACCTG file

	%^*** See if BY_LOGS option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-by_l*ogs:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Bylogs_Sts.

* When all of the qualifiers are absent this means the user wants to create all
* of the files. History does not default to Y, however: it must be specified
* explicitly.
	IF ((Failure_Is IN Dump_Statlogs_Sts)
	  AND (Failure_Is IN Dump_Prtlogs_Sts)
	  AND (Failure_Is IN Dump_Bylogs_Sts))
	THEN
	    SET Success_Is IN Dump_Statlogs_Sts TO true
	    SET Success_Is IN Dump_Prtlogs_Sts TO true
	    SET Success_Is IN Dump_Ants_Sts TO true
	    SET Success_Is IN Dump_Bylogs_Sts TO true
	END-IF.

* Note whether /DESTINATION given (wants output of BY_LOGS_DMP_DESTINATION.DAT file from
* the DST_DUMP routine).

	%^*** See if DESTINATION option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-dest*ination:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Dest_Sts.

* Note whether /HISTORY given (wants output file from Msg_hist_export routine).
* Per #5873
	%^*** See if HISTORY option is turned on
	Call "ACE_ARG_FIND" using
	      by content "-hist*ory:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Dump_History_Sts.

* Note whether /ITEXT given (wants output of BY_LOGS_DMP_PTEXT.DAT file for incoming payments text).

	Call "ACE_ARG_FIND" using
	      by content "-itext:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Dump_Itext_Sts.

* Note whether /OTEXT given (wants output of BY_LOGS_DMP_PTEXT.DAT file for outgoing payments text).

	Call "ACE_ARG_FIND" using
	      by content "-otext:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Otext_Sts.

* Note whether /MTEXT given (wants output of BY_LOGS_DMP_MTEXT.DAT file).
* This could trivially be expanded to output all messages for which any
* text was stored; for now only certain hard-wired types of interest to
* Trail are output.

	Call "ACE_ARG_FIND" using
	      by content "-mtext:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Mtext_Sts.

* Note whether /RTEXT given (wants output of BY_LOGS_DMP_RTEXT.DAT file).

	Call "ACE_ARG_FIND" using
	      by content "-rtext:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Rtext_Sts.

* Secondary text option, when rtext is requested, also export text from
*	dst_set.formatted_text_seq

	Call "ACE_ARG_FIND" using
	      by content "-dtext:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Dtext_Sts.


* Note whether /EDI qualifier given (wants output of edi remittance text).

	Call "ACE_ARG_FIND" using
	      by content "-edi",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Edi_Sts.


	Call "ACE_ARG_FIND" using
	      by content "-rgw:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Rgw_Sts.

* Note whether /TRAIL given (wants output of TRAIL_OUTPUT file).
* This could trivially be expanded to output all messages for which any
* text was stored; for now only certain hard-wired types of interest to
* Trail are output.

	Call "ACE_ARG_FIND" using
	      by content "-trail:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Dump_Trail_Sts.

* Determine whether message text is to be scanned for character replacement.
* In order for character replacement to take place this option must be
* present at execution time and there must be a substitution table in 
* cfg_tab.dat.

	Call "ACE_ARG_FIND" using
	      by content "-replace:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      RETURNING Replace_Char_Sts.

* Indicates whether or not to extract the processing rules 
* directly from a message or to use the PRULE API.
*
	Call "ACE_ARG_FIND" using
	      by content "-prule_d*irect:",
	      by content    "U",
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      by reference OMITTED,
	      returning Prule_direct_sts.

	If Success_is in Trace_sts
		display " "
		If Success_is in Prule_direct_sts
			display "Reading the Message Processing Rules directly from the objects"
		Else
			display "Using the API for reading the Message Processing Rules"
		End-if
		display " "
	End-if.

* Get the user supplied timestamp in case of backdating the time stamp
* of the dump records

	Call "ACE_ARG_FIND" using
	 by content "-timestamp_over*ride:"
	 by content "U"
	 by value %siz(User_timebuf_ws)
	 by reference User_timebuf_ws
	 by reference User_timebuf_ws_length
	 by reference omitted
	 by reference omitted
	 returning User_stamp_ws

	If Success_is in User_stamp_ws
	Then
	    If User_timebuf_ws_length not = 20
	    then
		Display "BY_LOGS$_DATE_ERROR - Incorrect length:"
		Display User_timebuf_ws, User_timebuf_ws_length
		%libstop "BY_LOGS$_BAD_DD_MMM_YYYY_HHMMSS";
	    End-if
	    %beg
	    Timestamp_Parse_ws
		 ^IN(User_timebuf_ws),
		 User_time_wrk.Dd,"-",User_time_wrk.Mon,"-",User_time_wrk.Yyyy,
		 " ",User_time_wrk.Hh,":",User_time_wrk.Mn,":",User_time_wrk.Ss,/;
	    %end
	    if failure_is in Timestamp_Parse_ws_status
            then
		Display "BY_LOGS$_DATE_ERROR - Bad format:"
		Display "TimeStamp Override Parse failed -", User_timebuf_ws
		%libstop "BY_LOGS$_BAD_DD_MMM_YYYY_HHMMSS";
	    else
		%^ move to 20 byte string variable in rgwlink_fsect
		Move User_timebuf_ws to Use_timestmp_ws

		%^ this will force a trap if there is still a problem
		%beg
		User_time_ws = Use_timestmp_ws;
		%end
	    end-if
	else
%^ no time entered - either continuous RGW or current timestamps are desired
	    Move spaces to Use_timestmp_ws
	end-if.
	    

* Get length of LIL_DMPREC so later it can be output directly from BIG_DMPREC.

	move length of LIL_DMPREC     to LIL_DMPREC_Length.


* Initialize our friend the single-message dumper.
* Note this fills the bank array, used immediately by A20_STAT_DUMP.
	Call "MSG_DUMP_INIT" using
		by reference	ERRPFX,
		by reference	ERRPFX_lgth_ws.

	%Beg
%^ Connection needed for the DAT_CONN_MSG call.
	dat_root_set (.Rel_acc_index CONN: Rel_acc_index);
	%End.

%^ connection needed by prule subs
	
	%ACE_CONN_ROOT_Q Cur_frx_index;.

	%Beg ALLOC_TEMP: Badlogtxt_q; %End.

A10_SBJ_INIT_end.
	exit.

A20_STAT_DUMP.
* Called once, to dump static-domain data to a separate small file.  Use
* of this file allows all message-domain report programs to be completely
* independent of the databases, so database saves, etc., can be initiated
* much earlier.	 Currently, only bank-id->bank-name correspondences, taken
* from the BANK_ARRAY table built by MSG_DUMP_INIT, are dumped into the file.

	If Failure_is IN Dump_Statlogs_Sts
	THEN
	    Go to A20_STAT_DUMP_end
	END-IF.

	Open Output STAT_DUMP_FILE.

	Move "B" to REC_TYPE					of STAT_DMPREC.

	Perform varying BANK_IDX from 1 by 1 until BANK_IDX > BANK_CNT
	    Move BANK_ID	of BANK_INFO(BANK_IDX) to STR4	of STAT_DMPREC
	    Move IDNAME		of BANK_INFO(BANK_IDX) to STR35	of STAT_DMPREC
	    Move DISTRICT	of BANK_INFO(BANK_IDX) to STR2	of STAT_DMPREC
	    Write STAT_DMPREC
	end-Perform.

	Close STAT_DUMP_FILE.
	%^***************************************************************************
	%^** BY_LOGS_DMP$_STATIDUMPED  /I Total number of banks in static dump: ${1}.
	%^***************************************************************************
	move BANK_CNT	     to BNK_COUNT.
	call "NEX_CREATE_AND_BROADCAST_MSG" using 
	      by content   Z"BY_LOGS_DMP$_STATIDUMPED"
	      by value -1
	      %ace_msg_arg_list(BNK_COUNT);.

A20_STAT_DUMP_end.
	exit.

A30_PRT_DUMP.

	If (Failure_Is IN Dump_Prtlogs_Sts)
	then
	   Go to A30_PRT_DUMP_end
	End-if.
	Open Output PRT_DUMP_FILE.

	%Beg QUE_ROOT_INDEX ^First; %End.
	Perform until failure_is in QUE_ROOT_INDEX_Status 
	    Move SPACES to IS_A_PRINTER

	    %Beg 
	    Parse ^IN(Que_root_Index.q_key.idname), IS_A_PRINTER, ^SPACE ;
	    %End
*  Offset back to index of position of third character from end.
	    SUBTRACT 2 from IS_A_PRINTER_LENGTH giving PRT_START
	    Perform E50_BNK_CHECK thru
		    E50_BNK_CHECK_END
	    If success_is in include_bank_ws
		    AND (Q_TYPE of QUE_ROOT_INDEX  = "QTYP$_LINELOG")
		    AND (IS_A_PRINTER(PRT_START:3) = "PRT" )
	    THEN
		Move 0 to MSG_COUNT
		Perform B10_PTR_LOG_DMP thru B10_PTR_LOG_DMP_end
		Perform X30_INFORM	thru X30_INFORM_end
	    end-If
	   %Beg QUE_ROOT_INDEX ^Next; %End
	end-Perform.

	Close PRT_DUMP_FILE.

	%^***********************************************************************
	%^** BY_LOGS_DMP$_PRTIDUMPED   /I Total number of printer messages: ${1}.
	%^***********************************************************************
	call "NEX_CREATE_AND_BROADCAST_MSG" using 
	      by content   Z"BY_LOGS_DMP$_PRTIDUMPED"
	      by value -1
	      %ace_msg_arg_list(TOT_MSG_COUNT);.

	%beg TOT_MSG_COUNT = <0>; %end.

A30_PRT_DUMP_end.
	exit.

A40_DUMP_INDEX.
* Called for REF_INDEX. Connect to it, then get right period if specified.
* Set period in BIG_DMPREC; then scan index, dump messages, report results.
* NOTE B20_BIG_DMP skips msgs with wrong bank, purely for REF_INDEX case.

%^***********************************************************************
%^*  BY_LOGS_DMP$_QUEROOT      /E Could not find ${1} in QUE_ROOT_INDEX
%^***********************************************************************

	%ace_conn_q ////"REF_INDEX" to INDEXA for read_only; 
	If failure_is in Ace_status_wf
	Then
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		  by content   Z"BY_LOGS_DMP$_QUEROOT"
		  by value -1
		  %ace_msg_arg_list(QUE_ROOT_INDEX.Q_KEY.idname);
	    %LIBSTOP "BY_LOGS_DMP$_QUEROOT";
	Else
	    %beg INDEXA ^First; %end
	End-if.

	%^*********************************************************************
	%^** The ref_index does no longer has a period, Therefore we must
	%^** process the ref_index using the trn date. Period only exist for
	%^** logs under the new structure
	%^*********************************************************************
	if rpt_date_yymmdd not = spaces
	THEN
	    %Beg INDEXA ^Search (forward, geq, key = trn_key_ws); %End
	    If failure_is in INDEXA_status or
		ref of INDEXA(3:6) not = rpt_date_yymmdd
	    then
%^*****************************************************************************
%^* BY_LOGS_DMP$_PERIODNF /I No messages for specified period in REF_INDEX. Period: ${1}
%^*****************************************************************************
		Display ">>>>>>**************************************"
		call "NEX_CREATE_AND_BROADCAST_MSG" using 
		  by content   Z"BY_LOGS_DMP$_PERIODNF"
		  by value -1
		  %ace_msg_arg_list(trn_key_ws.trn_date_ws);
		Display ">>>>>>**************************************"
		Move "Y" to end_ref_index_ws
	    end-If
	End-If.

	Move PERIOD_ws	   to MSG_PERIOD_ws.

	Perform until failure_is in INDEXA_Status
		      or end_ref_index_ws = "Y"
	    If Object_is in INDEXA_Cursor
	    then
		if ref of INDEXA(1:8) = Trn_date_ws
		Then
		    %Beg INDEXA Conn: MSG_HISTORY_SEQ; %End
		    Perform B20_BIG_DMP thru B20_BIG_DMP_end
		else
		    %beg end_ref_index_ws = "Y"; %end
		end_if
	    End-if
	   %Beg INDEXA ^Next; %End
	end-Perform.

       %Beg Break: INDEXA; %End.

A40_DUMP_INDEX_end.
	exit.

A50_DUMP_OPRACT.
* Called for each QUE_ROOT_NDX oper-action log (limited to 1 bank if specified).
* Connect to log, then get the right period if one was specified.
* Scan log, dump messages which are not in the current period, report results.

	%Beg QUE_ROOT_INDEX Conn: OPRACT ^First; %End.
	If failure_is in QUE_ROOT_INDEX_Status
	then
	   Go to A50_DUMP_OPRACT_end
	End-if.

	%Beg Change: OPRACT; %End.

	If PERIOD_ws not = Zero
	THEN
	  %Beg OPRACT ^Search_Period Period=PERIOD_ws; OPRACT ^First; %End
	  If OPRACT_Period not = PERIOD_ws
	  THEN
	     Set failure_is in OPRACT_Status to true
	  end-If
	End-If.

	Move OPRACT_Period		 to MSG_PERIOD_ws.
	%beg
	b30_chk_ws opr_act_is;
	%end


	Perform until failure_is in OPRACT_Status or
	    seq_end_is in OPRACT_Cursor
	    If Object_is in OPRACT_Cursor
	    then
		%^* need to check the opract.txt date str(6)
		%^* to determine is message is of current is equal to
		%^* current processing date. processing date
		%beg temp_vstr20_ws =  Opract.txt; %end
		SET Failure_Is IN Already_Process_Sts TO true
		PERFORM b30_chk_already_processed
		   THRU b30_chk_already_processed_end
		If (Failure_Is IN Already_Process_Sts)
		then
		    %Beg 
		    OPRACT (Notrap); 
		    OPRACT Conn: MSG_HISTORY_SEQ; 
		    %End
		    If success_is in OPRACT_Status
			Then
			Perform B20_BIG_DMP thru B20_BIG_DMP_end
		    End-if
		    %Beg 
		    OPRACT (Etrap); 
		    %End
		End-if
	    end-If
	    %Beg OPRACT ^Next; %End
	end-Perform.

       %Beg Break: OPRACT; %End.

A50_DUMP_OPRACT_end.
	exit.

A70_DUMP_AMT_BAL_LOG.
* Called for each QUE_ROOT_NDX FED{IN,OUT}_LOG (limited to 1 bank if specified).
* Connect to log, then get the right period if one was specified.
* Scan log, dump messages which are not in the current period, report results.

       %Beg QUE_ROOT_INDEX Conn: AMT_BAL_LOG ^First; %End.
	If failure_is in QUE_ROOT_INDEX_Status
	then
	    Go to A70_DUMP_AMT_BAL_LOG_end
	END-IF.

	%Beg Change: AMT_BAL_LOG; %End.

	If PERIOD_ws not = Zero
	THEN
	    %Beg AMT_BAL_LOG ^Search_Period Period=PERIOD_ws; AMT_BAL_LOG ^First; %End
	    If AMT_BAL_LOG_Period not = PERIOD_ws
	    THEN
	       Set failure_is in  AMT_BAL_LOG_Status to true
	    end-If
	End-If.

	Move AMT_BAL_LOG_Period to MSG_PERIOD_ws.
	%beg
	b30_chk_ws amt_bal_log_is;
	%end

	Perform until failure_is in AMT_BAL_LOG_Status or
	     seq_end_is in AMT_BAL_LOG_Cursor
	    If Object_is in AMT_BAL_LOG_Cursor
		%^* need to check the amt_bal_log.txt date str(6)
		%^* to determine is message is of current is equal to
		%^* current processing date. processing date
	    THEN
		SET Failure_Is IN Already_Process_Sts TO true
		%beg temp_vstr20_ws =  Amt_bal_log.txt; %end
		PERFORM b30_chk_already_processed
		   THRU b30_chk_already_processed_end
		IF (Failure_Is IN Already_Process_Sts)
		then
		    %Beg 
		    AMT_BAL_LOG (Notrap); 
		    AMT_BAL_LOG Conn: MSG_HISTORY_SEQ; 
		    %End
		    If success_is in AMT_BAL_LOG_Status
			Then
			Perform B20_BIG_DMP thru B20_BIG_DMP_end
		    End-if
		    %Beg 
		    AMT_BAL_LOG (Etrap); 
		    %End
		End-if
	    end-If
	   %Beg AMT_BAL_LOG ^Next; %End
	end-Perform.

       %Beg Break: AMT_BAL_LOG; %End.

A70_DUMP_AMT_BAL_LOG_end.
	exit.

A80_DUMP_GEN_VSTR_NDX.
* Called for each QUE_ROOT_NDX gen_vstr index  (limited to 1 bank if specified).
* Connect to log, then get the right period if one was specified.
* Scan log, dump messages which are not in the current period, report results.

	%Beg QUE_ROOT_INDEX Conn: GEN_VSTR_NDX ^First; %End.
	If failure_is in QUE_ROOT_INDEX_Status
	then
	   Go to A80_DUMP_GEN_VSTR_NDX_end
	End-if.

	%Beg
	Status_wf = GEN_VSTR_NDX State.periodic;
	change: GEN_VSTR_NDX;
	%End.

%^ Bypass the Gen_Vstr_Index if it's not in a periodic domain


	If PERIOD_ws not = Zero and Success_is in Status_wf
	THEN
	    %Beg 
	    GEN_VSTR_NDX ^Search_Period Period=PERIOD_ws; 
	    %End
	    If Success_is in GEN_VSTR_NDX_STATUS
	    and GEN_VSTR_NDX_Period = PERIOD_ws
	    Then
		%beg
	        GEN_VSTR_NDX ^First;
	   	%end
		Move GEN_VSTR_NDX_Period to MSG_PERIOD_ws
		%beg
		b30_chk_ws gen_vstr_index_is;
		%end
	    Else
		Set failure_is in GEN_VSTR_NDX_Status to true
	    End-If
	Else
%^ not periodic or period = 0
	    If failure_is in Status_wf
	    Then
		Set failure_is in GEN_VSTR_NDX_Status to true
	    End-if
	End-If.



	Perform until failure_is in GEN_VSTR_NDX_Status or
	    seq_end_is in GEN_VSTR_NDX_Cursor
	    If Object_is in GEN_VSTR_NDX_Cursor
	    then
		%^* need to check the gen_vstr_ndx.txt date str(6)
		%^* to determine is message is of current is equal to
		%^* current processing date. processing date
		%beg temp_vstr20_ws =  Gen_Vstr_Ndx.txt; %end
		SET Failure_Is IN Already_Process_Sts TO true
		PERFORM b30_chk_already_processed
		   THRU b30_chk_already_processed_end
		If (Failure_Is IN Already_Process_Sts)
		then
		    %Beg 
		    GEN_VSTR_NDX (Notrap); 
		    GEN_VSTR_NDX Conn: MSG_HISTORY_SEQ; 
		    %End
		    If success_is in GEN_VSTR_NDX_Status
		    Then
			Perform B20_BIG_DMP thru B20_BIG_DMP_end
		    End-if
		    %Beg 
		    GEN_VSTR_NDX (Etrap); 
		    %End
		End-if
	    end-If
	    %Beg GEN_VSTR_NDX ^Next; %End
	end-Perform.

       %Beg Break: GEN_VSTR_NDX; %End.

A80_DUMP_GEN_VSTR_NDX_end.
	exit.

A90_ADDITIONAL_QUEUES.
%^
%^ check config for additional queues to dump
%^
	%BEG
	Cfg_union_key_ws(
		.Idname	= "QUEUE_TABLES",
		.Idprod	= "MTS",
		.Idbank	= "",
		.Idloc	= "",
		.Idcust	= "" );
	Cfg_item_key_ws = "BY_LOGS_QUEUES:";
	%END.

	Call	"CFG_MATCH_ITEM"
	using	by reference	Idname of Cfg_union_key_ws
		by reference	Idprod of Cfg_union_key_ws
		by reference	Idbank of Cfg_union_key_ws
		by reference	Idloc of Cfg_union_key_ws
		by reference	Idcust of Cfg_union_key_ws
		by reference	Cfg_item_key_ws
		by reference	Idname of Q_key of Que_root_index
		by reference	Idname_LENGTH of Que_root_index_LENGTHS
		by reference	Cfg_seq_ordinal_ws
		by reference	Cfg_error_msg_ws
		by reference	Cfg_error_msg_ws_LENGTH
	returning		Cfg_status_wf.

	If success_is in Cfg_status_wf
	Then
	    Evaluate true
		When Q_TYPE of Que_root_index = "OPR_ACTION_LOG"
			Perform A50_DUMP_OPRACT thru
				A50_DUMP_OPRACT_END

		When Q_TYPE of Que_root_index = "AMT_BAL_LOG"
			Perform A70_DUMP_AMT_BAL_LOG thru
				A70_DUMP_AMT_BAL_LOG_END

		When Q_TYPE of Que_root_index = "GEN_VSTR_INDEX"
			Perform A80_DUMP_GEN_VSTR_NDX thru
				A80_DUMP_GEN_VSTR_NDX_END
	    End-evaluate
	End-if.

A90_ADDITIONAL_QUEUES_END.
	exit.

B10_PTR_LOG_DMP.
* Called for each QUE_ROOT_NDX printer linelog (limited to 1 bank if specified).
* Connect to log, then get the right period if one was specified.
* Scan log, dump messages to special printer dump file.

       %Beg QUE_ROOT_INDEX Conn: PRINTER ^First; %End.
	If failure_is in QUE_ROOT_INDEX_Status
	then
	    Go to B10_PTR_LOG_DMP_end
	END-IF.

	%Beg Change: PRINTER; %End.

	If PERIOD_ws not = Zero
	THEN
	    %Beg PRINTER ^Search_Period Period=PERIOD_ws; PRINTER ^First; %End
	    If PRINTER_Period not = PERIOD_ws
	    then
		%Beg Break: PRINTER; %End
		Go to B10_PTR_LOG_DMP_end
	    end-If
	end-If.

	%Beg
	   qprefix_vstr_ws  = NULL;
	   qsuffix_vstr_ws  = NULL;
	   PARSE  ^OneOf ((qprefix_vstr_ws,"PRT",/),		%^* null title
			  (qprefix_vstr_ws,"PRT",qsuffix_vstr_ws,/)); %^* title exists	 
	%end.
	If success_is in Parse_status
	Then
	    move qprefix_vstr_ws(1:qprefix_vstr_ws_length) to PRINTER of PRT_DMPREC
	Else
	    Move IDNAME of Q_KEY of QUE_ROOT_INDEX	   to PRINTER of PRT_DMPREC
	End-if.

	%beg Period_str = PRINTER Period.yyyymmdd; %end.
	Move Period_str to PERIODX of PRT_DMPREC.
%^sa	Move PRINTER_Period			to PERIODX	of PRT_DMPREC.

	Perform until failure_is in PRINTER_Status
	    If Object_is in PRINTER_Cursor
	    then
		%Beg PRINTER Conn: MSG_HISTORY_SEQ; %End
		Perform C10_PRT_DMP thru C10_PRT_DMP_end
	    end-If
	   %Beg PRINTER ^Next; %End
	end-Perform.
       %Beg Break: PRINTER; %End.

B10_PTR_LOG_DMP_end.
	exit.

B20_BIG_DMP.

* Connect to a message.
* If cannot connect, probably just old and got purged off - skip it.
* Fails immediately if caller's connect to MSG_HISTORY failed.
* If that's OK, check lots of other things. If all OK, dump msg and count it.

* Skip missing messages.
	If Failure_is in MSG_HISTORY_SEQ_Status
	then
	    Go to B20_BIG_DMP_CLEANUP
	END-IF.
	
* Skip messages in the skipped TRN sequence.
	%Beg
        Beg: Skip_trn_seq;
	Skip_trn_seq ^SEARCH (forward, eql, key = Indexa.ref);
        %End.

        If Success_is in Skip_trn_seq_status
	THEN
            DISPLAY "%BY_LOGS_DMP-W-SKIPTRN, TRN "
		Ref of Indexa(1:8) "-" Ref of Indexa(9:8)	
		" matches skip file entry - skipped"
            Go to B20_BIG_DMP_CLEANUP
        End-if.

* Connect message union and FTR set, skipping if a problem occurs.
	Perform X10_CONNECT_FTR thru X10_CONNECT_FTR_end.
	If Failure_is in Return_P_Status 
	THEN
	    Go to B20_BIG_DMP_CLEANUP
	end-if.

	If success_is in Trace_sts
		display "trn_ref = " trn_ref of msg_ftr_set
	End-if.

* Skip RBE batch-control messages.
%^ 110124
%^	If TRAN_TYPE of MSG_FTR_SET = "CTL"
%^	then
%^	    Go to B20_BIG_DMP_CLEANUP
%^	END-IF.

* Check if anticipation, and if so, whether wanted.
	If ((TRAN_TYPE of MSG_FTR_SET = "ANT")
	  AND (Failure_Is IN Dump_Ants_Sts))
	THEN
	    GO TO b20_big_dmp_cleanup
	END-IF.

* SPR 8506: BY_LOGS used to check bank here.  Moved a few lines down.

* Connect to DEBIT and CREDIT sets, skipping if a problem occurs.
	Perform X20_CONNECT_DBT_CDT thru X20_CONNECT_DBT_CDT_end.
	If Failure_is in Return_P_Status 
	THEN
	    Go to B20_BIG_DMP_CLEANUP
	end-if.

* We are going to dump this message.  Count it, and connect the confirmation
* sequences.
	Add 1 to MSG_COUNT.

* Equate history subjects to get the prules sequences connected.
	%beg
	msg_history_seq equate: ent_msg_history(nomod);
	%end.

	Call "DAT_CONN_MSG".

%^ 30-OCT-2002 DFN - Commented out. They are now obsolete.
%^	%Beg
%^	Msg_debit_set.Cnf_seq CONN: Msg_dr_cnf_seq ^FIRST;
%^	Msg_credit_set.Cnf_seq CONN: Msg_cr_cnf_seq ^FIRST;
%^	%End.

* Check whether bank specified, and if so, whether message matches.

	Move BANK of MSG_FTR_SET to bnk_chk_ws.
	Perform E50_BNK_CHECK thru
		E50_BNK_CHECK_END.

	If failure_is in include_bank_ws
	Then
	    Move Cdt_adr_bnk_id of Msg_credit_set to bnk_chk_ws
	    Perform E50_BNK_CHECK thru
		    E50_BNK_CHECK_END
	End-if.

	If success_is in include_bank_ws
%^        If (BNK_STR = spaces or
%^                      BANK of MSG_FTR_SET or
%^                      Cdt_adr_bnk_id of Msg_credit_set)
	Then

* If scanning FEDOUT_LOG or SECOUT_LOG, reconnect message history to
* escape embedded dest-seq.
%^ 118460 - this is done in x10_connect_for_ftr

%^	    If Idname of Q_key of QUE_ROOT_INDEX = "FEDOUT_LOG" or
%^	            Idname of Q_key of QUE_ROOT_INDEX = "SECOUT_LOG"
%^	    then
%^	       %Beg
%^		Break: MSG_HISTORY_SEQ;
%^		MSG_UNION.MSG_HISTORY Conn: MSG_HISTORY_SEQ;
%^	       %End
%^	    end-If

* Call our friend to fill in the big dump record, in our private format ("0").
* If the Dump_history flag is set call msg_dump with the extra parameter
* containing the address of the call back subroutine Msg_hist_export. Per #5873

%^*	    If Dump_history Not = "Y" then
%^*		Move 0 to Dmp_rtn_addr 
%^*	    End-if

* If the DUMP_DESTINATION flag is set, call MSG_DUMP with the extra parameter
* containing the address of the call back subroutine DST_DUMP.

%^*            If DUMP_DESTINATION Not = "Y" then
%^*                Move 0 to Dst_rtn_addr
%^*            End-if

	    Move 9 to Arg_Number
	    CALL "MSG_DUMP" USING
	      BY REFERENCE Arg_Number
	      BY REFERENCE Big_Dmprec
	      BY CONTENT "0"
	      BY REFERENCE Msg_Period_Ws
	      BY REFERENCE Dump_History_Sts
	      BY REFERENCE Dump_Dest_Sts
	      BY REFERENCE Dest_Ordinal_Ws
	      BY REFERENCE Acctg_Done_Ws
	      BY REFERENCE Use_timestmp_ws
	      BY REFERENCE Rec_Length_Ws		%^argument not used
							%^ here in by logs dmp



* Write out BIG_DMPREC and LIL_DMPREC.

	    Write BIG_DMPREC
	    Write LIL_DMPREC from BIG_DMPREC(1:LIL_DMPREC_Length)
	    If (Success_Is IN Rgw_Sts)
	    then
		Move ASC_NOW_TIMESTAMP of BIG_DMPREC to Now_timestamp_str
		%beg 
		Now_timestamp_ws = Now_timestamp_str; 
%^		work_date.yymmdd = msg_ftr_set.trn_ref.trn_date;
%^		Trn_date_rgw = work_date.yyyymmdd;
		Trn_date_rgw = msg_ftr_set.trn_ref.trn_date;
		Trn_num_rgw = msg_ftr_set.trn_ref.trn_num;
		%end

		Initialize				   RGW_MSG_CR_DMPREC
		Move corresponding BIG_DMPREC		to RGW_MSG_CR_DMPREC
		Perform Z801_CR_SETUP thru Z801_CR_SETUP_END
		Move "N"				to Record_Updated of RGW_MSG_CR_DMPREC
		Move trn_date_rgw			to trn_date of RGW_MSG_CR_DMPREC
		Move Trn_num_rgw			to Trn_num of RGW_MSG_CR_DMPREC
		Write RGW_MSG_CR_DMPREC

		Initialize				   RGW_MSG_DR_DMPREC
		Move corresponding BIG_DMPREC		to RGW_MSG_DR_DMPREC
		Perform Z802_DR_SETUP thru Z802_DR_SETUP_END
		Move "N"				to Record_Updated of RGW_MSG_DR_DMPREC
		Move trn_date_rgw			to trn_date of RGW_MSG_DR_DMPREC
		Move Trn_num_rgw			to Trn_num of RGW_MSG_DR_DMPREC
		Write RGW_MSG_DR_DMPREC

		Initialize				   RGW_LIL_DMPREC
		Move  corresponding BIG_DMPREC		to RGW_LIL_DMPREC
		Perform Z808_LIL_SETUP thru Z808_LIL_SETUP_END
                move msg_type of BIG_DMPREC             to msg_type of rgw_lil_dmprec

		Move "N"				to Record_Updated of RGW_LIL_DMPREC
		Move trn_date_rgw to trn_date of TRN_REF of RGW_LIL_DMPREC

		Move "FTR" to Cust_mask_key_func_ws
		Call "CUST_MASK_TESTKEY" using
			by reference Cust_mask_key_func_ws,
			by reference RGW_LIL_DMPREC
			by reference RGW_RTEXT_DMPREC
			returning return_status

		Write RGW_LIL_DMPREC

* prule_direct bypasses the processing rule API to gather processing rule data
		If Failure_is in prule_direct_sts
			Move zeroes to Prule_seq_num_ws
			Perform Z900_PRULES thru Z900_PRULES_end
		Else
			Move zeroes to Prule_seq_num_ws
			%Beg 
			MSG_CREDIT_SET(NoTrap,
				.Cdt_proc_rule CONN: msg_proc_rule ^FIRST);
			MSG_CREDIT_SET(ETrap);
			%end
			%ACE_IS msg_proc_rule CONNECTED giving prule_conn_sts;

			If success_is in prule_conn_sts
			Then
			    Move corresponding RGW_MSG_CR_DMPREC to RGW_MSG_PR_DMPREC
			    Perform Z950_PRULE_SEQ thru Z950_PRULE_SEQ_end
			End-if

			%Beg 
			MSG_DEBIT_SET(NoTrap,
				.dbt_proc_rule CONN: msg_proc_rule ^FIRST); 
			MSG_DEBIT_SET(ETrap);
			%end
			%ACE_IS msg_proc_rule CONNECTED giving prule_conn_sts;

			If success_is in prule_conn_sts
			Then
			    Move corresponding RGW_MSG_DR_DMPREC to RGW_MSG_PR_DMPREC
			    Perform Z950_PRULE_SEQ thru Z950_PRULE_SEQ_end
			End-if

			%Beg 
			MSG_DEBIT_SET(NoTrap,
				.sbk_proc_rule CONN: msg_proc_rule ^FIRST); 
			MSG_DEBIT_SET(ETrap);
			%end
			%ACE_IS msg_proc_rule CONNECTED giving prule_conn_sts;

			If success_is in prule_conn_sts
			Then
			    Move corresponding RGW_MSG_DR_DMPREC to RGW_MSG_PR_DMPREC
			    Perform Z950_PRULE_SEQ thru Z950_PRULE_SEQ_end
			End-if

			%Beg 
			MSG_FTR_SET(NoTrap,
				.ftr_proc_rule CONN: msg_proc_rule ^FIRST); 
			MSG_FTR_SET(ETrap);
			%end
			%ACE_IS msg_proc_rule CONNECTED giving prule_conn_sts;

			If success_is in prule_conn_sts
			Then
			    Move corresponding RGW_MSG_DR_DMPREC to RGW_MSG_PR_DMPREC
			    Perform Z950_PRULE_SEQ thru Z950_PRULE_SEQ_end
			End-if
		End-if
	    End-if

* Write the message to the TRAIL file if requested.  Check that accounting has
* been done and that the message has not been cancelled.  Also, check to see that
* it is not a split, or that if it is a split, it is the value date (second) half
* of the split.

	    If (Success_Is IN Dump_Trail_Sts)
	    then
		If (STS_ACC of Big_dmprec = "Y")
		  and (STS_CAN of BIG_DMPREC not = "Y") and
		    ((TRAN_TYPE of MSG_FTR_SET not = "FFR") and
		     (TRAN_TYPE of MSG_FTR_SET not = "FFS") and 
		     (TRAN_TYPE of MSG_FTR_SET not = "SPL"))
		    or
		    ((TRAN_TYPE of MSG_FTR_SET = "SPL") and
		     (PROC_DATE of BIG_DMPREC = VALUE_DATE of BIG_DMPREC))
		then
			Initialize				   TRAIL_DMPREC
			Move corresponding BIG_DMPREC		to TRAIL_DMPREC
			Perform Z809_TRAIL_SETUP thru Z809_TRAIL_SETUP_END
			Write TRAIL_DMPREC
			Add +1					to TRAIL_REC_COUNT
		end-If
	    end-If

* Write out the incoming and outgoing payments text if requested.

	    If (Success_Is IN Dump_Itext_Sts)
	      or (Success_Is IN Dump_Otext_Sts)
	    then
		Perform C20_PTEXT_DMP thru C20_PTEXT_DMP_end
	    end-If

* Write out the message text if requested.

	    If (Success_Is IN Dump_Mtext_Sts)
	    then
		Perform C30_MTEXT_DMP thru C30_MTEXT_DMP_end
	    end-If

* Write out the received text if requested.

	    If (Success_Is IN Dump_Rtext_Sts)
	    then
		Perform C40_RTEXT_DMP thru C40_RTEXT_DMP_end
	    end-If


*	If the -rgw argument was present output to the file for 
*		related amount data.

	    If (Success_is in rgw_sts)
		then
		    Move "W" to Rel_amt_mode_ws
		    Call "RELATED_AMT_DUMP" using 
			By Reference Rel_amt_mode_ws
			By Reference Big_dmprec
			By Reference Big_dmprec_len_ws
			By Reference Use_timestmp_ws
	    End-if



* Write out the accounting records to the accounting file.

	    Perform E10_FIND_BANK thru E10_FIND_BANK_end

* Write the records to the "accounting" file.
	    If Sts_acc of Big_dmprec = "Y"
	    THEN
		Move "W" to Acctg_status_ws
		call "ACCTG_DUMP" using
	       		by reference Acctg_status_ws
	      		by reference Acctg_file_name_ws
			by reference Big_dmprec
			by reference Big_dmprec_len_ws
			by reference Acctg_rec_count
			by reference Use_timestmp_ws
	    End-if
	End-if.

%^
%^ something was written - add trn to BY_LOGS_TRN_OUTPUT
%^ so that that file can be used as a skip list in the event
%^ of a file size limit problem that causes a crash

	Move Trn_ref of Msg_ftr_set to Trnlist_rec.
	Move 16 to Trnlist_rec_len.
	Write Trnlist_rec.

b20_big_dmp_cleanup.

	Call "DAT_BREAK_MSG".

       %Beg
	BREAK: MSG_HISTORY_SEQ;
	BREAK: MSG_UNION;
	BREAK: MSG_FTR_SET;
	BREAK: MSG_DEBIT_SEQ;
	BREAK: MSG_DEBIT_SET;
%^	BREAK: MSG_DR_CNF_SEQ;
	BREAK: MSG_CREDIT_SEQ;
	BREAK: MSG_CREDIT_SET;
%^	BREAK: MSG_CR_CNF_SEQ;
	BREAK: MSG_RELATED_AMOUNT_SEQ;
       %End.

B20_BIG_DMP_end.
	EXIT.

B25_BUILD_TRN_SKIP_LIST.
* The file is scanned, and any correctly formatted TRN's (i.e. 12 digit numbers with or
* without a "-" between columns 6 and 7) are inserted in the list of TRN's to be skipped.

	%Beg
        Alloc_temp: Skip_trn_seq (mod);
	%End.

        Set Success_is in Skiplist_file_status to True.
        Open Input SKIPLIST.
	If SKIPLIST_FILE_STAT not = "00"
	THEN
	    Set Failure_is in Skiplist_file_status to True
	End-if.

        Perform Until Failure_is in Skiplist_file_status

            Read Skiplist into Skip_trn_rec
                At end  CLOSE Skiplist
                        Go to B25_BUILD_TRN_SKIP_LIST_end
            End-read
            Move Skiplist_rec_len to Skip_trn_rec_length

	    %Beg
	    Parse ^NOTRAP ^IN(Skip_trn_rec), 
		Skip_trn_seq (.Skip_trn_key.Date_part (^STRING<8> (<CHAR$M_NUMBER>))),
              ^OPTION ("-"),
	        Skip_trn_seq (.Skip_trn_key.Trn_part (^STRING<8> (<CHAR$M_NUMBER>))), /;
          %End
          If Success_is in Parse_status 
	  THEN
              %Beg ALLOC_ELEM: Skip_trn_seq; %End
          End-if

        End-perform.

B25_BUILD_TRN_SKIP_LIST_end.
	EXIT.
B30_CHK_ALREADY_PROCESSED.

	%^************************************************************
	%^*** following code is all new to replace diddle processing *
	%^*** in B20_BIG_DMP					     *
	%^************************************************************

	Set Failure_is in Badlogtxt_data_ws to TRUE.
	If (Temp_vstr20_ws_length not = 16 and 12)
		Set Success_is in Badlogtxt_data_ws to TRUE
*   Attempt to connect to the message union to determine the message number
		Perform X15_CONNECT_FOR_TRN thru X15_CONNECT_FOR_TRN_END
		%Beg Compose ^OUT(Temp_vstr_ws), Que_root_index.Q_key.Idname,/; %End
		Perform X40_CHECK_QUE thru X40_CHECK_QUE_END
	End-if. 

	If Success_is in Badlogtxt_data_ws
		If Failure_is in Badlogtxt_noted_ws
			call "NEX_CREATE_AND_BROADCAST_MSG" using 
		      					    by content   Z"BY_LOGS_DMP$_BADLOGTXT"
							    by value -1
							    %ace_msg_arg_list(Temp_vstr_ws, temp_vstr20_ws);
		End-if

		If Failure_is in CONNECT_RETURN_STATUS
*   Give up and consider the message to be a duplicate
			SET Success_Is IN Already_Process_Sts TO true
			GO TO B30_CHK_ALREADY_PROCESSED_END
		End-if
	End-if.

%^ we have some form of trn - check new or old format
%^ and bail if there is anything weird.

	If temp_vstr20_ws_length equal 16 
	THEN
	    Perform B35_16_CHAR_CHK through
		    B35_16_CHAR_CHK_END
	Else
%^ check old 12 byte format
	    if temp_vstr20_ws_length = 12 
	    then
	        Perform B35_12_CHAR_CHK through
		        B35_12_CHAR_CHK_END
	    else
%^ give up - it's trash
		SET Success_Is IN Already_Process_Sts TO true		
	    End-if
	END-IF.

B30_CHK_ALREADY_PROCESSED_END.
	exit.

B35_16_CHAR_CHK.
	If temp_vstr20_ws(1:8) = Trn_date_ws 
%^* means processed today;
	THEN
	    SET Success_Is IN Already_Process_Sts TO true		
	ELSE
	    %beg
	    Chk_compose ^out(txt16_ws) temp_vstr20_ws,/;
	    Chk_parse ^in(txt16_ws),
			tmp_trn_date,
			tmp_trn_num,/;
	    %end
	    If success_is in Chk_compose_status and
		    success_is in Chk_parse_status
	    Then
		%beg
		Chk_compose ^out(ws_new_trn)
			tmp_trn_date, tmp_trn_num, /;
		%end
		Perform B35_CHK_FOR_DUP through
			B35_CHK_FOR_DUP_END
	    Else
%^ give up it's junk
		SET Success_Is IN Already_Process_Sts TO true
	    END-IF
	End-if.

B35_16_CHAR_CHK_END.
	exit.

B35_12_CHAR_CHK.
	if temp_vstr20_ws(1:6) = rpt_date_yymmdd 	
%^* means processed today;
	then
	    SET Success_Is IN Already_Process_Sts TO true		
	else
	    %beg
	    Chk_compose ^out(txt12_ws) temp_vstr20_ws,/;
	    Chk_parse ^in(txt12_ws),
			tmp_trn_date6,
			tmp_trn_num6,/ ;
	    %end
	    If success_is in Chk_compose_status and
		success_is in Chk_parse_status
	    Then
		%beg
		tmp_date.yymmdd = tmp_trn_date6;
		Chk_compose ^out(ws_new_trn)
		   tmp_date.yyyymmdd, "00", tmp_trn_num6, /;
		%end
	    	Perform B35_CHK_FOR_DUP through
		    	B35_CHK_FOR_DUP_END
	    Else
%^ give up it's junk
		SET Success_Is IN Already_Process_Sts TO true
	    End-if
	End-if.

B35_12_CHAR_CHK_END.
	exit.

B35_CHK_FOR_DUP.

%^* check next input function for dup 
	If success_is in Chk_compose_status
	Then
	    %beg
	    trndup_index ^Search (key=ws_new_trn);
	    %end

	    If success_is in trndup_index_status 
%^** message already processed we can skip it
	    then
	        SET Success_Is IN Already_Process_Sts TO true
	    else
	        %beg Alloc_elem: trndup_index (.ref = ws_new_trn); %end
	    End-if
	Else
%^ give up the trn is trash
	    SET Success_Is IN Already_Process_Sts TO true
	End-if.

B35_CHK_FOR_DUP_END.
	exit.

C10_PRT_DMP.
	If Failure_is in MSG_HISTORY_SEQ_status
	THEN
	    GO TO c10_prt_dmp_cleanup
	End-if.

* Connect message union and FTR set, exiting if a problem occurs.
	Perform X10_CONNECT_FTR thru X10_CONNECT_FTR_END.

* Check for correct bank.
	Move BANK of MSG_FTR_SET to bnk_chk_ws.
	Perform E50_BNK_CHECK thru
		E50_BNK_CHECK_END.

	If failure_is in include_bank_ws
%^	If not (BNK_STR = spaces or BANK of MSG_FTR_SET)
	THEN
	    GO TO c10_prt_dmp_cleanup
	END-IF.

* Connect to DEBIT and CREDIT sets, exiting if a problem occurs.
	Perform X20_CONNECT_DBT_CDT thru X20_CONNECT_DBT_CDT_END.

* We are going to dump this message: Connect rest of it, dump it, and count it.
	Perform D10_COPY	thru D10_COPY_end.
	Perform D20_WIRETYPE	thru D20_WIRETYPE_end.
	Perform D30_ACCTG	thru D30_ACCTG_end.
	Perform D40_STS_ACC	thru D40_STS_ACC_end.

	Write PRT_DMPREC.
	Add 1 to MSG_COUNT.

* Break message subjects we connected. Note: harmless to break unconnected ones.

c10_prt_dmp_cleanup.

       %BEG
	Break: MSG_HISTORY_SEQ;
	Break: MSG_UNION;
	Break: MSG_FTR_SET;
	Break: MSG_DEBIT_SEQ;
	Break: MSG_DEBIT_SET;
	Break: MSG_CREDIT_SEQ;
	Break: MSG_CREDIT_SET;
	Break: MSG_RELATED_AMOUNT_SEQ;
       %End.

C10_PRT_DMP_end.
	exit.

C20_PTEXT_DMP.
* Dump incoming (when Dump_itext = "Y") and outgoing (when Dump_otext = "Y")
* payments text using the PTEXT format.

* Move in TRN fields from BIG_DMPREC.

*	Move Trn_date   of Big_dmprec		to Trn_cent	of Ptext_dmprec.
	Move Trn_num of Trn_ref	of Big_dmprec	to Trn_num	of Ptext_dmprec.

* If dumping the incoming text, try to connect the message text sequence.
* If present and not empty, dump it.

	If (Success_Is IN Dump_Itext_Sts)
	then
	    %Beg
	    Msg_union (notrap);
	    Msg_union.Txt CONN: Msg_text_seq(notrap);
	    %End

	    If Success_is in Msg_text_seq_status 
	    THEN
		%beg
		Msg_text_seq ^first;
		%end
		Move "I" to Text_type of Ptext_dmprec
		Move 0 to Ptext_sequence_ws
		Perform until Seq_end_is in Msg_text_seq_cursor
		    Add 1 to Ptext_sequence_ws
		    Move Ptext_sequence_ws to Convert_long_num
		    Move Convert_long_str to Sequence_no of Ptext_dmprec
		    Move Txt of Msg_text_seq to Msg_text of Ptext_dmprec

		    Write Ptext_dmprec
		    Add 1 to Ptext_rec_count

		    %Beg  NEXT: Msg_text_seq;  %End
		end-perform
	    end-if

	    %Beg
	    BREAK: Msg_text_seq;
	    Msg_union (etrap);
	    %End
	end-if.

* If dumping the outgoing text, try to connect the credit payment and
* secondary wire sequences.  For each that is present and not empty,
* dump it.

	If (Success_Is IN Dump_otext_Sts)
	then
	    %Beg
	    Msg_credit_set (notrap);
	    Msg_credit_set.Pay_seq CONN: Msg_cr_pay_seq(notrap);
	    %End

	    If Success_is in Msg_cr_pay_seq_status
	    THEN
		%beg
		Msg_cr_pay_seq ^first;
		%end
		Move "O" to Text_type of Ptext_dmprec
		Move 0 to Ptext_sequence_ws
		Perform until seq_end_is in Msg_cr_pay_seq_cursor
		    Add 1 to Ptext_sequence_ws
		    Move Ptext_sequence_ws to Convert_long_num
		    Move Convert_long_str to Sequence_no of Ptext_dmprec
		    Move Msg_cr_pay_seq to Msg_text of Ptext_dmprec

		    Write Ptext_dmprec
		    Add 1 to Ptext_rec_count

		    %Beg  NEXT: Msg_cr_pay_seq;  %End
		end-perform
	    end-if

	    %Beg
	    BREAK: Msg_cr_pay_seq;
	    BREAK: Msg_cr_sec_seq;
	    Msg_credit_set.Secwir_seq CONN: Msg_cr_sec_seq(notrap);
	    %End

	    If Success_is in Msg_cr_sec_seq_status
	    THEN
		%beg
		Msg_cr_sec_seq ^first;
	 	%end

		Move "S" to Text_type of Ptext_dmprec
		Move 0 to Ptext_sequence_ws
		Perform until seq_end_is in Msg_cr_sec_seq_cursor
		    Add 1 to Ptext_sequence_ws
		    Move Ptext_sequence_ws to Convert_long_num
		    Move Convert_long_str to Sequence_no of Ptext_dmprec
		    Move Txt of Msg_cr_sec_seq to Msg_text of Ptext_dmprec

		    Write Ptext_dmprec
		    Add 1 to Ptext_rec_count

		    %Beg  NEXT: Msg_cr_sec_seq;  %End
		end-perform
	    end-if

	    %Beg
	    BREAK: Msg_cr_sec_seq;
	    Msg_credit_set (etrap);
	    %End
	end-if.


	If (Success_Is IN Dump_Edi_sts)
		%^ Connect to the edit remittance text
	
		%beg 
		msg_union (.remittance_txt CONN: msg_remittance_txt_seq(nomod,notrap));
		%end


		If Success_Is IN msg_remittance_txt_seq_STATUS 
		   %^Text sequence is Allocated, see if and data
		   %Beg First: msg_remittance_txt_seq; %End

		   If seq_end_is in msg_remittance_txt_seq_cursor 

		      %^ No data present, exit
		      %Beg Break: msg_remittance_txt_seq; %End
		   Else
		      %^ Data is present write to msg_text 
		      Perform C25_Ptext_Edi Thru C25_Ptext_Edi_End
		   end-if

		else
		   %^ Not allocated, just exit
		   %beg break: msg_remittance_txt_seq; %end

	     	   %beg
	  	   compose ^out(vstr2),  "******* NO REMITTANCE TEXT *******", /;
		   %end 
		   Move vstr2 to  Msg_text of Ptext_dmprec
		   Write Ptext_dmprec
		   Add 1 to Ptext_rec_count
		end-if
	End-if.


C20_PTEXT_DMP_end.
	EXIT.

C25_PTEXT_EDI.


	%^ EDI data is present, output data :

	%beg
	compose ^out(vstr2),  "     EDI REMITTANCE TEXT        ", /;
	%end 
	Move vstr2 to  Msg_text of Ptext_dmprec
	Write Ptext_dmprec
	Add 1 to Ptext_rec_count


	%^ Get the length of the data.

	%Beg parse ^IN(msg_remittance_txt_seq.txt) "/",Edi_type_ws(^STR<4>),"/",Vstr1,/; 
	%end

	Add Vstr1_length to Long1.
	
	Perform Until (seq_end_is in msg_remittance_txt_seq_cursor ) or
			(Failure_is in msg_remittance_txt_seq_STATUS)
		Add txt_length in msg_remittance_txt_seq_lengths to Long1
		%Beg Next: msg_remittance_txt_seq; %End
	End-perform.

	%beg
	compose ^out(vstr2), "EDI Data Length: ",Long1,/;
	%end 
	Move vstr2 to  Msg_text of Ptext_dmprec
	Write Ptext_dmprec
	Add 1 to Ptext_rec_count

	Evaluate Edi_type_ws
		When "UEDI"
			%Beg compose ^out(vstr2),  "EDI Data type is UN-EDIFACT", /; %End
			Move vstr2 to  Msg_text of Ptext_dmprec
			Write Ptext_dmprec
			Add 1 to Ptext_rec_count
		
		When "ANSI"
			%Beg compose ^out(vstr2), "EDI Data type is ANSI_X12",/; %End
			Move vstr2 to  Msg_text of Ptext_dmprec
			Write Ptext_dmprec
			Add 1 to Ptext_rec_count

		When "SWIF"
			%Beg compose ^out(vstr2), "EDI Data type is SWIFT",/; %End
			Move vstr2 to  Msg_text of Ptext_dmprec
			Write Ptext_dmprec
			Add 1 to Ptext_rec_count

		When "NARR"
			%Beg compose ^out(vstr2), "EDI Data type is NARRATIVE",/; %End
			Move vstr2 to  Msg_text of Ptext_dmprec
			Write Ptext_dmprec
			Add 1 to Ptext_rec_count

	End-Evaluate


	%Beg First: msg_remittance_txt_seq; %End

	Perform Until (Seq_end_is in msg_remittance_txt_seq_cursor ) or
		(Failure_is in msg_remittance_txt_seq_STATUS)

		%Beg
		compose ^out(vstr2) msg_remittance_txt_seq.txt,/;
		%end
		Move vstr2 to  Msg_text of Ptext_dmprec
		Write Ptext_dmprec
		Add 1 to Ptext_rec_count

		%beg
		NEXT: msg_remittance_txt_seq;
		%End
	End-perform

	%Beg
		Break: msg_remittance_txt_seq;
	%End.



C25_PTEXT_EDI_END.
	EXIT.


C30_MTEXT_DMP.
* Dump message text. Called only when DUMP_MTEXT="Y".
* Decide whether to output a record based on MSG_UNION MSGTYPE, FTR.TYPE_CODE.
* If so, big dump record just written, so BIG_DMPREC fields are all set, but
* the FTR's currency and amount are empty for these message types, so those
* fields must be parsed from the message text. Finally, the message text must
* be composed into the output record's big field (via a vstr buffer). Its
* lines are delimited by <CR><LF>, because that's what Trail wants.
*
* typ description (from SWFPRINT.COB)	     sources of fields
* --- -------------------------------------- ----------------------------------
* 100 Customer Xfer 			     :32A:yymmddcccnn...,[mm]
* 200 Bank Xfer Own Account		     same as 100
* 201 Multi-Bank Xfer Own Account	     :19:nn...,[mm] :32B:ccc :30:yymmdd 
* 202 Bank Xfer In Favor of 3rd Bank	     same as 100
* 203 Multi-Bank xfer In Favor of 3rd Bank   same as 201

* Skip it if it's not a type Trail is interested in.

	If (STS_ACC of BIG_DMPREC = "Y")
	or (not ADMIN in MSGTYPE of MSG_UNION)
        or (Int_reroute_flg of Flgs2 of Msg_ftr_set = "Y" )
	or (not TRAN_TYPE of TYP of MSG_FTR_SET = "SWF")
	or (not (TYPE_CODE of MSG_FTR_SET =
		"100 " or "103 " or "200 " or "201 " or "202 " or "203 "))
	then
	    Go to C30_MTEXT_DMP_end
	end-If.

* Copy useful fields from BIG_DMPREC.

	MOVE Trn_Date of Trn_ref OF Big_Dmprec TO Trn_Date OF Mtext_Dmprec.
	MOVE Trn_Num of Trn_ref OF Big_Dmprec TO Trn_Num OF Mtext_Dmprec.
	MOVE Bank_Id OF Big_Dmprec TO Bank_Id OF Mtext_Dmprec.
	MOVE Loc OF Big_Dmprec TO Loc OF Mtext_Dmprec.
	MOVE Inst_Date OF Big_Dmprec(3:6) TO Inst_Date OF Mtext_Dmprec.
	MOVE Src_Code OF Big_Dmprec TO Src_Code OF Mtext_Dmprec.

* Set fields to default values in case cannot parse text for some reason.

	MOVE 0 TO Amt OF Mtext_Dmprec.
	MOVE 0 TO For_Amt OF Mtext_Dmprec.
	MOVE spaces TO Currency_Code OF Mtext_Dmprec.
	MOVE spaces TO Msg_Text OF Mtext_Dmprec.
	MOVE spaces TO Value_Date OF Mtext_Dmprec.

* Try to connect message text seq.
* If present and not empty, parse it based on type.

       %Beg MSG_UNION.TXT Conn: MSG_TEXT_SEQ(notrap);  %End.

	If Success_is in MSG_TEXT_SEQ_STATUS
	THEN
	   %Beg MtParse ^IN(MSG_TEXT_SEQ.Txt); %End
	    If TYPE_CODE of MSG_FTR_SET = "201 " or "203 "
	    then
		Perform D50_PARSE_MULTI  thru D50_PARSE_MULTI_end
	    else
		Perform D60_PARSE_SINGLE thru D60_PARSE_SINGLE_end
	    end-If
	    Perform E10_FIND_BANK thru E10_FIND_BANK_end
	    If CURRENCY_CODE of MTEXT_DMPREC =
	       BASE_CURRENCY of BANK_INFO(BANK_IDX)
	    then
		Move FOR_AMT of MTEXT_DMPREC to AMT of MTEXT_DMPREC
	    else
		Move 0 to AMT of MTEXT_DMPREC
	    end-If
	    Perform D70_COMPOSE_TEXT thru D70_COMPOSE_TEXT_end
	   %Beg Break: MSG_TEXT_SEQ; %End
	end-If.

	Write MTEXT_DMPREC.
	Add 1 to MTEXT_REC_COUNT.

C30_MTEXT_DMP_end.
	EXIT.

C40_RTEXT_DMP.
* Dump message text. Called only when DUMP_RTEXT="Y".
* Dump all the text as received in the TXT object.

* Copy useful fields from BIG_DMPREC.

	Move TRN_DATE of Trn_ref of BIG_DMPREC to TRN_DATE of RTEXT_DMPREC.
	Move TRN_NUM of Trn_ref of BIG_DMPREC to TRN_NUM of RTEXT_DMPREC.
	Move 0				 to RTEXT_SEQUENCE_ws.

* Try to connect message text seq.
* If present and not empty, dump it.

       %Beg MSG_UNION.TXT Conn: MSG_TEXT_SEQ(notrap); %End.

	If success_is in MSG_Text_seq_Status then
	    %beg
	    MSG_TEXT_SEQ ^first;
	    %end
	    Move "I"			to Text_type of Rtext_dmprec
	    Move 0			to RTEXT_SEQUENCE_ws
	    Perform until seq_end_is in MSG_TEXT_SEQ_Cursor
		Add 1 to RTEXT_SEQUENCE_ws
		Move RTEXT_SEQUENCE_WS	to Long_string
		Move Long_string	to SEQUENCE_NO	of RTEXT_DMPREC

		If Success_is in Replace_char_sts
			%beg
        	        repl_chars_in_str = msg_text_seq.txt;
			%end

*	Scan through the text for characters that must be replaced.
*	This will replace any characters that cannot be loaded into RGW tables

			Call "NEX_TO_PRINTABLE" using
				by reference repl_chars_in_str,
				by value repl_chars_in_str_length

			Move repl_chars_in_str	to MSG_TEXT	of RTEXT_DMPREC
		Else
			Move Txt of MSG_TEXT_SEQ to MSG_TEXT	of RTEXT_DMPREC
		End-if

		Move Now_timestamp_str 	to Asc_now_timestamp of RTEXT_DMPREC
		Write RTEXT_DMPREC
		Add 1 to RTEXT_REC_COUNT
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Move "TXT" to Cust_mask_key_func_ws
		   Call "CUST_MASK_TESTKEY" using
			by reference Cust_mask_key_func_ws,
			by reference RGW_LIL_DMPREC
			by reference RGW_RTEXT_DMPREC
			returning return_status

		   Write RGW_RTEXT_DMPREC
		End-if

	       %Beg Next: MSG_TEXT_SEQ; %End
	
	    end-Perform
	END-IF.	
	
       %Beg Break: MSG_TEXT_SEQ; %End.

* If dumping the outgoing text, try to connect the credit payment and
* secondary wire sequences.  For each that is present and not empty,
* dump it.

	%Beg
	Msg_credit_set (notrap);
	Msg_credit_set.Pay_seq CONN: Msg_cr_pay_seq(notrap);
	%End.

	If success_is in Msg_cr_pay_seq_status then
	    %beg
	    Msg_cr_pay_seq ^first;
	    %end
	    Move "O" to Text_type of Rtext_dmprec
	    Move 0 to Rtext_sequence_ws
	    Perform until seq_end_is in Msg_cr_pay_seq_cursor

		Add 1 to Rtext_sequence_ws
		Move RTEXT_SEQUENCE_WS	to Long_string
		Move Long_string	to SEQUENCE_NO	of RTEXT_DMPREC
		Move Txt of Msg_cr_pay_seq to Msg_text	of Rtext_dmprec
		Move Now_timestamp_str 	to Asc_now_timestamp of RTEXT_DMPREC
		Write RTEXT_DMPREC
		Add 1 to RTEXT_REC_COUNT
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Write RGW_RTEXT_DMPREC
		End-if
		%Beg  NEXT: Msg_cr_pay_seq;  %End
	    end-perform
	end-if.

	%Beg
	BREAK: Msg_cr_pay_seq;
	BREAK: Msg_cr_sec_seq;
	Msg_credit_set.Secwir_seq CONN: Msg_cr_sec_seq(notrap);
	%End.

	If success_is in Msg_cr_sec_seq_status then
	    %beg
	    Msg_cr_sec_seq ^first;
	    %end
	    Move "S" to Text_type of Rtext_dmprec
	    Move 0 to Rtext_sequence_ws

	    Perform until seq_end_is in Msg_cr_sec_seq_cursor

		Add 1 to Rtext_sequence_ws
		Move RTEXT_SEQUENCE_WS	to Long_string
		Move Long_string	to SEQUENCE_NO	of RTEXT_DMPREC
		Move Txt of Msg_cr_sec_seq to Msg_text	of Rtext_dmprec
		Move Now_timestamp_str 	to Asc_now_timestamp of RTEXT_DMPREC
		Write RTEXT_DMPREC
		Add 1 to RTEXT_REC_COUNT
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Write RGW_RTEXT_DMPREC
		End-if

		%Beg  NEXT: Msg_cr_sec_seq;  %End
	    end-perform
	    %beg
	    BREAK: Msg_cr_sec_seq(etrap);
	    %end
	end-if.

	If (Success_Is IN Dump_Edi_sts)
	%^ Connect to the edit remittance text
	
	   %beg 
	   msg_union (.remittance_txt CONN: msg_remittance_txt_seq(nomod,notrap));
	   %end


	If Success_Is IN msg_remittance_txt_seq_STATUS 
		%^Text sequence is Allocated, see if and data
		%Beg First: msg_remittance_txt_seq; %End

		If seq_end_is in msg_remittance_txt_seq_cursor 

		 %^ No data present, exit
		 %Beg Break: msg_remittance_txt_seq; %End
		Else
		 %^ Data is present write to msg_text 
		 Perform C45_Rtext_Edi Thru C45_Rtext_Edi_End
		end-if

	else
		%^ Not allocated, just exit
		%beg break: msg_remittance_txt_seq; %end
	     	   %beg
	  	   compose ^out(vstr2),  "******* NO REMITTANCE TEXT *******", /;
		   %end 

		   Move vstr2 to  Msg_text of Rtext_dmprec
		   Write RTEXT_DMPREC
		   Add 1 to Rtext_rec_count
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Write RGW_RTEXT_DMPREC
		End-if

	End-if.


* Generate destination formatted_text_seq records


	If (Success_Is IN Dump_Dtext_sts)
* Search for destination sets, through dst_seq
* Connect to the formatted text sequence if there is one

	    %Beg
	    Msg_union.dst_seq CONN: msg_dst_seq (notrap);
	    %End

	    If success_is in msg_dst_seq_status
	    Then
		Perform C41_GET_DST_SETS thru
			C41_GET_DST_SETS_END

	    End-if

	    %beg
	    BREAK: msg_dst_seq;
	    %end

	    MOVE 0	TO DST_ORDINAL_NUM
	    Move Dst_ordinal_ws to Dst_ordinal	of RTEXT_DMPREC

	End-If.


C40_RTEXT_DMP_end.
	EXIT.
C41_GET_DST_SETS.

	%beg
	msg_dst_seq ^first;
	%end.

	Perform until (seq_end_is IN msg_dst_seq_cursor)

	    %beg
	    msg_dst_seq CONN: msg_dst_set (notrap);
	    %end
	    If success_is in msg_dst_set_status
	    Then
		Perform C42_GET_FMT_TXT thru
			C42_GET_FMT_TXT_END
	    End-if

	    %beg
	    BREAK: msg_dst_set;
	    NEXT: msg_dst_seq;
	    %end

	End-Perform.




C41_GET_DST_SETS_END.
	EXIT.
C42_GET_FMT_TXT.

	%beg
	msg_dst_set.Formatted_text_seq 
		CONN: Msg_Formatted_text_seq(notrap);
	%end.

	If success_is in Msg_Formatted_text_seq_status
	Then
	    %beg
	    Msg_Formatted_text_seq ^first;
	    %end

	    If not Seq_end_is in Msg_Formatted_text_seq_cursor
	    Then
	        Move "D" to Text_type of Rtext_dmprec
		Move 0 to Rtext_sequence_ws
		MOVE Dst_ordinal OF Msg_dst_set 
				TO DST_ORDINAL_NUM

		Perform until seq_end_is in Msg_Formatted_text_seq_cursor
		    Add 1 to Rtext_sequence_ws
		    Move RTEXT_SEQUENCE_WS to Long_string
		    Move Long_string to SEQUENCE_NO of RTEXT_DMPREC
		    Move Dst_ordinal_ws to Dst_ordinal of RTEXT_DMPREC
		    Move Txt of Msg_Formatted_text_seq	to Msg_text of Rtext_dmprec
		    Move Now_timestamp_str to Asc_now_timestamp of RTEXT_DMPREC
		    Write RTEXT_DMPREC
		    Add 1 to RTEXT_REC_COUNT
		    If (Success_Is IN Rgw_Sts)
		    then
			Initialize RGW_RTEXT_DMPREC
			Move corresponding RTEXT_DMPREC to RGW_RTEXT_DMPREC
			Perform Z807_RTEXT_SETUP thru
				Z807_RTEXT_SETUP_END
			Move "N" to Record_Updated of RGW_RTEXT_DMPREC
			Write RGW_RTEXT_DMPREC
		    End-if


		    %Beg
		    NEXT: Msg_Formatted_text_seq;
		    %End

		End-Perform

	    End-if
	End-if.

	%beg
	BREAK: Msg_Formatted_text_seq;
	%end.
	Move 0 to Rtext_sequence_ws.	


C42_GET_FMT_TXT_END.
	EXIT.
C45_RTEXT_EDI.


	%^ EDI data is present, output data :

	%beg
	compose ^out(vstr2),  "     EDI REMITTANCE TEXT        ", /;
	%end 
	Move vstr2 to  Msg_text of rtext_dmprec
	Write rtext_dmprec
	Add 1 to Rtext_rec_count
		
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Write RGW_RTEXT_DMPREC
		End-If

	%^ Get the length of the data.

	%Beg parse ^IN(msg_remittance_txt_seq.txt) "/",Edi_type_ws(^STR<4>),"/",Vstr1,/; 
	%end

	Add Vstr1_length to Long1.
	
	Perform Until (seq_end_is in msg_remittance_txt_seq_cursor ) or
			(Failure_is in msg_remittance_txt_seq_STATUS)
		Add txt_length in msg_remittance_txt_seq_lengths to Long1
		%Beg Next: msg_remittance_txt_seq; %End
	End-perform.

	%beg
	compose ^out(vstr2), "EDI Data Length: ",Long1,/;
	%end 
	Move vstr2 to  Msg_text of rtext_dmprec
	Write rtext_dmprec
	Add 1 to Rtext_rec_count
		If (Success_Is IN Rgw_Sts)
		then
		   Initialize				   RGW_RTEXT_DMPREC
		   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
		   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
		   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
		   Write RGW_RTEXT_DMPREC
		End-If

	Evaluate Edi_type_ws
		When "UEDI"
			%Beg compose ^out(vstr2),  "EDI Data type is UN-EDIFACT", /; %End
			Move vstr2 to  Msg_text of rtext_dmprec
			Write rtext_dmprec
			Add 1 to Rtext_rec_count
			If (Success_Is IN Rgw_Sts)
			then
			   Initialize				   RGW_RTEXT_DMPREC
			   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
			   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
			   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
			   Write RGW_RTEXT_DMPREC
			End-If
		
		When "ANSI"
			%Beg compose ^out(vstr2), "EDI Data type is ANSI_X12",/; %End
			Move vstr2 to  Msg_text of rtext_dmprec
			Write rtext_dmprec
			Add 1 to Rtext_rec_count
			If (Success_Is IN Rgw_Sts)
			then
			   Initialize				   RGW_RTEXT_DMPREC
			   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
			   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
			   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
			   Write RGW_RTEXT_DMPREC
			End-If

		When "SWIF"
			%Beg compose ^out(vstr2), "EDI Data type is SWIFT",/; %End
			Move vstr2 to  Msg_text of rtext_dmprec
			Write rtext_dmprec
			Add 1 to Rtext_rec_count
			If (Success_Is IN Rgw_Sts)
			then
			   Initialize				   RGW_RTEXT_DMPREC
			   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
			   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
			   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
			   Write RGW_RTEXT_DMPREC
			End-If

		When "NARR"
			%Beg compose ^out(vstr2), "EDI Data type is NARRATIVE",/; %End
			Move vstr2 to  Msg_text of rtext_dmprec
			Write rtext_dmprec
			Add 1 to Rtext_rec_count
			If (Success_Is IN Rgw_Sts)
			then
			   Initialize				   RGW_RTEXT_DMPREC
			   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
			   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
			   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
			   Write RGW_RTEXT_DMPREC
			End-If

	End-Evaluate


	%Beg First: msg_remittance_txt_seq; %End

	Perform Until (Seq_end_is in msg_remittance_txt_seq_cursor ) or
		(Failure_is in msg_remittance_txt_seq_STATUS)

		%Beg
		compose ^out(vstr2) msg_remittance_txt_seq.txt,/;
		%end
		Move vstr2 to  Msg_text of rtext_dmprec
		Write rtext_dmprec
		Add 1 to Rtext_rec_count
		If (Success_Is IN Rgw_Sts)
			then
			   Initialize				   RGW_RTEXT_DMPREC
			   Move corresponding RTEXT_DMPREC	to RGW_RTEXT_DMPREC
			   Perform Z807_RTEXT_SETUP thru Z807_RTEXT_SETUP_END
			   Move "N"				to Record_Updated of RGW_RTEXT_DMPREC
			   Write RGW_RTEXT_DMPREC
		End-If

		%beg
		NEXT: msg_remittance_txt_seq;
		%End
	End-perform

	%Beg
		Break: msg_remittance_txt_seq;
	%End.



C45_RTEXT_EDI_END.
	EXIT.


D10_COPY.
* Copy basic fields, for printer dump.
	Move LAST_SEQ	   of PRINTER	     to Long_string.
	Move Long_string		     to PRTSEQ	  of PRT_DMPREC.
%^sa	Move LAST_SEQ	   of PRINTER	     to PRTSEQ	  of PRT_DMPREC.
	Move BANK	   of MSG_FTR_SET    to BANK_ID	  of PRT_DMPREC.

%^	Move TRN_DATE of trn_ref of MSG_FTR_SET    to Mtext_date.
%^	%beg
%^	    work_date.yymmdd = msg_ftr_set.trn_ref.trn_date;
%^	    yyyymmdd_DATE    = work_date.yyyymmdd;
%^	%end.
	move TRN_DATE of trn_ref of MSG_FTR_SET   to TRN_DATE  of PRT_DMPREC.
	Move TRN_NUM of trn_ref of MSG_FTR_SET    to TRN_NUM	  of PRT_DMPREC.

	Move BASE_AMOUNT   of MSG_FTR_SET    to ASCII_AMT.
	Move STR18_AMT			     to AMT		 of PRT_DMPREC.
%^sa	Move BASE_AMOUNT   of MSG_FTR_SET    to AMT		 of PRT_DMPREC.
	Move CURRENCY_CODE of MSG_FTR_SET    to CURRENCY_CODE	 of PRT_DMPREC.

	Move AMOUNT	   of MSG_FTR_SET    to ASCII_AMT.
	Move STR18_AMT			     to FOR_AMT		 of PRT_DMPREC.
%^sa	Move AMOUNT	   of MSG_FTR_SET    to FOR_AMT		 of PRT_DMPREC.
	Move CDT_NAME1	   of MSG_CREDIT_SET to CDT_NAME1	 of PRT_DMPREC.
	Move DBT_NAME1	   of MSG_DEBIT_SET  to DBT_NAME1	 of PRT_DMPREC.

D10_COPY_end.
	exit.

* Determine (site-specific) WIRE_TYPE via call to INTRTL, for printer dump.
D20_WIRETYPE.
* Note, if Cust_wiretype does not find entry, it sets Wire_type as spaces.
	Call "CUST_WIRETYPE" using
		by reference	    OMITTED,	%^ no error messages please
		by reference	    SRC_CODE	of MSG_FTR_SET,
		by reference	    CDT_ADV_TYP of MSG_CREDIT_SET,
		by reference	    WIRE_TYPE	of PRT_DMPREC
		returning RETURN_STATUS.

	If failure_is in RETURN_STATUS
	then
	    Move spaces	to WIRE_TYPE	of PRT_DMPREC
	End-if.

D20_WIRETYPE_end.
	exit.

* Determine credit and debit accounts, for printer dump.

D30_ACCTG.

* This paragraph fills the CDT_ACCTG and DBT_ACCTG fields.
* They are NOT exactly the same as their namesakes in the CREDIT and DEBIT sets.
* This moves in the correct fed g/l account value - either the federal reserve 
* or a special TT&L account created for irs 1090's  This value 
* will only occur on the credit side. (for now)

	Call "FILL_ACCTG" using
		CDT_IDTYPE	      of MSG_CREDIT_SET,
		CDT_ID		      of MSG_CREDIT_SET,
		IDKEY  of CDT_ACCOUNT of MSG_CREDIT_SET,
		IDTYPE of CDT_ACCOUNT of MSG_CREDIT_SET,
		CDT_ACCTG	      of PRT_DMPREC.

	If  ACCOUNT of CDT_ACCTG of PRT_DMPREC = spaces
	and CDT_IDTYPE of MSG_CREDIT_SET = "A"
	then
	    Perform E10_FIND_BANK thru E10_FIND_BANK_end

		%beg parse ^in(msg_credit_set.cdt_typ.cdt_id),
			^oneof( (^string<3>, ":", ws_Account,/),
				(ws_Account,/));
		%end

		If ws_account = ws_irs_account_no
		AND ((TTL_IDTYPE of BANK_INFO(BANK_IDX) not = spaces) 
		AND  (TTL_IDKEY OF BANK_INFO(BANK_IDX) not = spaces))
		then	  
			Move TTL_IDTYPE of BANK_INFO(BANK_IDX) to IDTYPE
						of CDT_ACCTG	of PRT_DMPREC
			Move TTL_IDKEY	of BANK_INFO(BANK_IDX) to ACCOUNT
						of CDT_ACCTG	of PRT_DMPREC

		Else
			Move FRB_IDTYPE of BANK_INFO(BANK_IDX) to IDTYPE
						of CDT_ACCTG	of PRT_DMPREC
			Move FRB_IDKEY	of BANK_INFO(BANK_IDX) to ACCOUNT
						of CDT_ACCTG	of PRT_DMPREC
		End-if
	end-If.

	Call "FILL_ACCTG" using
		DBT_IDTYPE		of MSG_DEBIT_SET,
		DBT_ID			of MSG_DEBIT_SET,
		IDKEY  of DBT_ACCOUNT	of MSG_DEBIT_SET,
		IDTYPE of DBT_ACCOUNT	of MSG_DEBIT_SET,
		DBT_ACCTG		of PRT_DMPREC.

	If ACCOUNT of DBT_ACCTG of PRT_DMPREC = spaces
	and DBT_IDTYPE of MSG_DEBIT_SET = "A"
	then
	    Perform E10_FIND_BANK thru E10_FIND_BANK_end
	    Move FRB_IDTYPE of BANK_INFO(BANK_IDX) to IDTYPE
						of DBT_ACCTG	of PRT_DMPREC
	    Move FRB_IDKEY  of BANK_INFO(BANK_IDX) to ACCOUNT
						of DBT_ACCTG	of PRT_DMPREC
	end-If.

D30_ACCTG_end.
	exit.

* Determine whether accounting was done for message, for printer dump.
D40_STS_ACC.

* Determine accounting status of tran type.
	Call "CUST_TRAN_TYPE" using
		by reference TRAN_TYPE of MSG_FTR_SET
		by reference ACCOUNTING_FLAG
		by reference BALANCE_CHECK_FLAG
		by reference TRADE_FLAG
		 returning RETURN_STATUS.

	If (failure_is in ACCOUNTING_FLAG)
	or (CDT_IDTYPE of MSG_CREDIT_SET = " ")
	or (SRC_CODE of MSG_FTR_SET = "MEM")
	then
	    Move "N" to STS_ACC of PRT_DMPREC
	else
	    Move "Y" to STS_ACC of PRT_DMPREC
	end-If.

D40_STS_ACC_end.
	exit.


D50_PARSE_MULTI.

       %Beg
	First: MSG_TEXT_SEQ;  MtParse ^Beg ^String
	":19:" MTEXT_AMT(^Number(^European_Format, ^Commas, ^NoDollar_Sign));
       %End.
	If failure_is in MtParse_Status
	then
	    Go to D50_PARSE_MULTI_end
	END-IF.

	If MTEXT_AMT > 999999999999.99
	then
	    Move 999999999999.99 to ASCII_AMT
	else
	    Move MTEXT_AMT to ASCII_AMT
	end-If.
	Move STR18_AMT to FOR_AMT of MTEXT_DMPREC.

       %Beg
	First: MSG_TEXT_SEQ;  MtParse ^Beg ^String
	":32B:" MTEXT_CUR(^String<-1>);
       %End.
	If failure_is in MtParse_Status
	THEN
	    Go to D50_PARSE_MULTI_end
	END-IF.

	Move MTEXT_CUR			 to CURRENCY_CODE of MTEXT_DMPREC.

       %Beg
	First: MSG_TEXT_SEQ;  MtParse ^Beg ^String
	":30:" MTEXT_DATE(^String<-1>);
       %End.
	If failure_is in MtParse_Status
	THEN
	    Go to D50_PARSE_MULTI_end
	END-IF.
	%beg
	    work_date.yymmdd = mtext_date;
	    yyyymmdd_DATE    = work_date.yyyymmdd;
	%end.
	move yyyymmdd_DATE(3:6)		 to VALUE_DATE	  of MTEXT_DMPREC.

D50_PARSE_MULTI_end.
	exit.

D60_PARSE_SINGLE.
       %Beg
	First: MSG_TEXT_SEQ;  MtParse ^Beg ^String
	":32A:"
	MTEXT_DATE(^String<-1>)
	MTEXT_CUR (^String<-1>)
	MTEXT_AMT(^Number(^European_Format, ^Commas, ^NoDollar_Sign));
       %End.
	If failure_is in MtParse_Status
	THEN
	    Go to D60_PARSE_SINGLE_end
	END-IF.
	%beg
	    work_date.yymmdd = mtext_date;
	    yyyymmdd_DATE    = work_date.yyyymmdd;
	%end.
	move yyyymmdd_DATE(3:6)		 to VALUE_DATE	  of MTEXT_DMPREC.
	Move MTEXT_CUR			 to CURRENCY_CODE of MTEXT_DMPREC.

	If MTEXT_AMT > 999999999999.99
	then
	    Move 999999999999.99 to ASCII_AMT
	else
	    Move MTEXT_AMT to ASCII_AMT
	end-If.
	Move STR18_AMT to FOR_AMT of MTEXT_DMPREC.

D60_PARSE_SINGLE_end.
	exit.

D70_COMPOSE_TEXT.
* Note the Compose stops the loop if it overflows, leaving BUF_ws_length 0.
       %Beg First: MSG_TEXT_SEQ; MtCompose ^Out(MTEXT_BUF); %End.

	Perform until failure_is in MSG_TEXT_SEQ_Status
		or    failure_is in MtCompose_Status
	   %Beg MtCompose MSG_TEXT_SEQ.TXT <29>; Next: MSG_TEXT_SEQ; %End
	end-Perform.

	If failure_is in MtCompose_Status
	then
	    Move 2000 to MTEXT_BUF_Length
	    Move "..." to MTEXT_BUF(1998:3)
	else
	   %Beg MtCompose /; %End
	end-if.

	Move MTEXT_BUF(1:MTEXT_BUF_Length) to MSG_TEXT	  of MTEXT_DMPREC.

D70_COMPOSE_TEXT_end.
	exit.

E10_FIND_BANK.
* Find FTR's bank in bank-info table set up by MSG_DUMP_INIT.
* Optimization for 99.99-percent case: BANK_IDX already is the right bank.
* Purposeful subtlety here: if no matches, returns BANK_IDX = last bank-index!
* Yes, I thought about the BANK_CNT=0 case, but it will never happen.

	If BANK_ID of BANK_INFO(BANK_IDX) not = BANK of MSG_FTR_SET
	THEN
	    Move 1 to BANK_IDX
	    Perform until	BANK_IDX = BANK_CNT
		or BANK_ID of BANK_INFO(BANK_IDX) = BANK of MSG_FTR_SET
		Add 1 to BANK_IDX
	    end-Perform

	end-If.

E10_FIND_BANK_end.
	exit.

E50_BNK_CHECK.
%^
%^ possibles -   args[n]   args[n]
%^ 		no bank, no phase - include_bank_ws = true
%^			I.E. - include all banks
%^		bank, no phase - include_bank_ws = 
%^				(bnk_str match on bnk_chk_ws = true)
%^			I.E. - include only this bank
%^		no bank, phase - include_bank_ws =
%^				(phase_bnk_seq match on bnk_chk_ws = true)
%^			I.E. - include bank belonging to this phase
	Evaluate True
	When (bnk_str = spaces and phs_str = spaces)
	    set success_is in include_bank_ws to true
	When (bnk_str not = spaces)
	    If (bnk_str = bnk_chk_ws)
		set success_is in include_bank_ws to true
	    Else
		set failure_is in include_bank_ws to true
	    End-if
	When (phs_str not = spaces)
	    %ACE_IS Phase_bnk_seq connected;
	    If success_is in Ace_status_wf
	    Then
	        %beg
	        SEARCH: Phase_bnk_seq (key = bnk_chk_ws);
	        %end
	        If success_is in Phase_bnk_seq_status
	        Then
		    set success_is in include_bank_ws to true
	        Else
		    set failure_is in include_bank_ws to true
	        End-if
	    Else
		set success_is in include_bank_ws to true
	    End-if
	When Other
	    Set failure_is in include_bank_ws to true
	End-evaluate.

E50_BNK_CHECK_END.
	exit.

X10_CONNECT_FTR.
* Starting from a successfully connected message history, connect the message
* union and FTR set, exiting if a problem occurs.

	Set Success_is in Return_P_Status to True.
	%Beg
	Msg_history_seq TOP: Msg_union (notrap,
	    .Ftr CONN: Msg_ftr_set (notrap) );
	Msg_union_sts = Msg_union STATUS;
	Msg_ftr_sts = Msg_ftr_set STATUS;
	Msg_union (etrap); 
	Msg_ftr_set (etrap);
	%End.
	If ((Failure_is in Msg_union_sts) or
	    (Failure_is in Msg_ftr_sts))
	then
	    %Ace_is Indexa connected giving Return_Status;
	    MOVE Return_Status TO Return_P_Status
	    If Success_is in Return_Status
	    then
		Display "%BY_LOGS_DMP-E-BADMSG, error connecting FTR set for TRN "
		    Ref of Indexa(1:6) "-" Ref of Indexa(7:6)
		    Skipping_str
	    else
		Display "%BY_LOGS_DMP-E-BADMSG, error connecting FTR set for unknown TRN"
		    Skipping_str
	    end-if
	    If Skip_bad_msg_str = Spaces
	    then
	    	%Libstop "%BY_LOGS_DMP-E-BADMSG, error connecting FTR set for unknown TRN";
	    else
		Set Failure_is in Return_P_Status to True
	    end-if
	end-if.

* 129642 - Check actual trn date in case the log text is defective
	If success_is in chk_subhist_conn_ws and
		trn_date of trn_ref of msg_ftr_set = trn_date_ws of trn_key_ws
		Set Failure_is in Return_P_Status to True
		Go to X10_CONNECT_FTR_end
	End-if.

%^
%^ 118460 - check for coming in on a log joined to subhistory
%^
	If success_is in chk_subhist_conn_ws
	    %beg
	    MSG_HISTORY_SEQ ^FIRST;
	    %end
	    If idname of qname of Msg_history_seq = "*DST"
	    Then
	        %Beg
	        Break: MSG_HISTORY_SEQ;
	        MSG_UNION.MSG_HISTORY Conn: MSG_HISTORY_SEQ;
	        %End
	    End-if
	End-if.

X10_CONNECT_FTR_end.
	EXIT.

X15_CONNECT_FOR_TRN.
* Alternate method for getting the message number.
* This is used when there is no TRN_REF in the text of the OPR_ACTION log.
* Attempt to connect to the message union and get it from the FTR_SET.

	Set Success_is in CONNECT_RETURN_STATUS to true.

	Evaluate true
		When amt_bal_log_is in b30_chk_ws

			%Beg 
			AMT_BAL_LOG (Notrap); 
			AMT_BAL_LOG Conn: MSG_HISTORY_SEQ; 
			%End
* All traces of a message have been lost, there is no message
*	to connect to and the trn_ref is unknown

			If ((Failure-is in AMT_BAL_LOG_status) or
				(Failure-is in MSG_HISTORY_SEQ_status))
			then
				Set Failure_is in CONNECT_RETURN_STATUS to true
			End-if
			%Beg
			AMT_BAL_LOG (Etrap); 
			%End

		When opr_act_is in b30_chk_ws
			%Beg 
			OPRACT (Notrap); 
			OPRACT Conn: MSG_HISTORY_SEQ; 
			%End
* All traces of a message have been lost, there is no message
*	to connect to and the trn_ref is unknown
			If ((Failure-is in OPRACT_status) or
				(Failure-is in MSG_HISTORY_SEQ_status))
			then
				Set Failure_is in CONNECT_RETURN_STATUS to true
			End-if
			%Beg
			OPRACT (Etrap); 
			%End

		When gen_vstr_index_is in b30_chk_ws
			%Beg 
			GEN_VSTR_NDX (Notrap); 
			GEN_VSTR_NDX Conn: MSG_HISTORY_SEQ; 
			%End
* All traces of a message have been lost, there is no message
*	to connect to and the trn_ref is unknown
			If ((Failure-is in GEN_VSTR_NDX_status) or
				(Failure-is in MSG_HISTORY_SEQ_status))
			then
				Set Failure_is in CONNECT_RETURN_STATUS to true
			End-if
			%Beg
			GEN_VSTR_NDX (Etrap); 
			%End

	End-evaluate.

	If Success_is in CONNECT_RETURN_STATUS
	Then
	    %Beg
	    Msg_history_seq TOP: Msg_union (notrap,
		    .Ftr CONN: Msg_ftr_set (notrap) );
	    Msg_union_sts = Msg_union STATUS;
	    Msg_ftr_sts = Msg_ftr_set STATUS;
	    Msg_union (etrap); 
	    Msg_ftr_set (etrap);
	    %End

	    If ((Failure-is in MSG_HISTORY_SEQ_status) or
		(Failure_is in Msg_union_sts) or
		(Failure_is in Msg_ftr_sts))
	    then
		Set Failure_is in CONNECT_RETURN_STATUS to true
	    else
		%beg 
		Chk_Compose ^out(temp_vstr20_ws) Msg_ftr_set.trn_ref.trn_date,Msg_ftr_set.trn_ref.trn_num,/;
		%end
		If Failure_is in Chk_Compose_status
		then
		    Set Failure_is in CONNECT_RETURN_STATUS to true
		End-if

	        %Beg
		BREAK: MSG_HISTORY_SEQ;
		BREAK: MSG_UNION;
		BREAK: MSG_FTR_SET;
	        %End
	End-if.

X15_CONNECT_FOR_TRN_END.
	EXIT.

X20_CONNECT_DBT_CDT.
* Starting from a successfully connected message union and FTR set, connect
* the DEBIT and CREDIT sets, exiting if a problem occurs.

	Set Success_is in Return_P_Status to True.
	%Beg
	Msg_union (notrap,
	    .Dbt_seq CONN: Msg_debit_seq (notrap),
	    .Cdt_seq CONN: Msg_credit_seq (notrap) );
	Msg_union_sts = Msg_union STATUS;
	Msg_debit_seq ^FIRST CONN: Msg_debit_set (notrap);
	Msg_debit_sts = Msg_debit_set STATUS;
	Msg_credit_seq ^FIRST CONN: Msg_credit_set (notrap);
	Msg_credit_sts = Msg_credit_set STATUS;
	Msg_union (etrap); 
	Msg_debit_seq (etrap); 
	Msg_debit_set (etrap); 
	Msg_credit_seq (etrap); 
	Msg_credit_set (etrap);
	%End.
	If Failure_is in Msg_union_sts
	then
	    Display "%BY_LOGS_DMP-E-BADMSG, error connecting DEBIT or CREDIT sequence for TRN "
		Trn_date of trn_ref of Msg_ftr_set "-" Trn_num of trn_ref of Msg_ftr_set
		Skipping_str
	    If Skip_bad_msg_str = Spaces
	    then
	    	%Libstop "%BY_LOGS_DMP-E-BADMSG, error connecting DEBIT or CREDIT sequence";
	    else
		Set Failure_is in Return_P_Status to True
	    end-if
	end-if.
	If Failure_is in Msg_debit_sts
	then
	    Display "%BY_LOGS_DMP-E-BADMSG, error connecting DEBIT set for TRN "
		Trn_date of trn_ref of Msg_ftr_set "-" Trn_num of trn_ref of Msg_ftr_set
		Skipping_str
	    If Skip_bad_msg_str = Spaces
	    then
	    	%Libstop "%BY_LOGS_DMP-E-BADMSG, error connecting DEBIT set";
	    else
		Set Failure_is in Return_P_Status to True
	    end-if
	end-if.
	If Failure_is in Msg_credit_sts
	then
	    Display "%BY_LOGS_DMP-E-BADMSG, error connecting CREDIT set for TRN "
		Trn_date of trn_ref of Msg_ftr_set "-" Trn_num of trn_ref of Msg_ftr_set
		Skipping_str
	    If Skip_bad_msg_str = Spaces
	    then
	    	%Libstop "%BY_LOGS_DMP-E-BADMSG, error connecting CREDIT set";
	    else
		Set Failure_is in Return_P_Status to True
	    end-if
	end-if.

X20_CONNECT_DBT_CDT_end.
	EXIT.


X30_INFORM.
* Some common code: Give info message if some messages were dumped from a log.
* MSG_COUNT has number msgs dumped; QUE_ROOT_NDX_Label has log and bank names.
* Also accumulates grand total of messages dumped.
	If MSG_COUNT > 0
	then
	    Move IDBANK of Q_key of QUE_ROOT_INDEX to BANK_IDENT
	    If BANK_IDENT = spaces
	    then
		Move " * " to BANK_IDENT
	    end-If %^refidx
	    %^********************************************************************************
	    %^* BY_LOGS_DMP$_BANKMCNT	  /I ${1} messages from Bank ${2} for queue ${3}.
	    %^********************************************************************************
	    call "NEX_CREATE_AND_BROADCAST_MSG" using 
		  by content   Z"BY_LOGS_DMP$_BANKMCNT"
		  by value -1
		  %ace_msg_arg_list(MSG_COUNT,BANK_IDENT,QUE_ROOT_INDEX.Q_KEY.IDNAME);
	    Add MSG_COUNT to TOT_MSG_COUNT
	end-If.

X30_INFORM_end.
	exit.

X40_CHECK_QUE.
*
* See if the queue name is already in our list of bad queues.  If it is then return
* success so we don't emit another BADLOGTXT message. Otherwise add it to out list
* and return failure so we do emit a BADLOGTXT message.
*
	%Beg SEARCH: Badlogtxt_q EQL key=Temp_vstr_ws; %End.
	If Failure_is in Badlogtxt_q_status
		%Beg
		ALLOC_ELEM: Badlogtxt_q.Vstr_key = Temp_vstr_ws;
		%End
		Set Failure_is in Badlogtxt_noted_ws to TRUE
	Else
		Set Success_is in Badlogtxt_noted_ws to TRUE
	End-if.

X40_CHECK_QUE_END.
	Exit.

Z801_CR_SETUP.

	Move Now_timestamp_ws to Now_timestamp of RGW_MSG_CR_DMPREC.

	Move Sec_flg of Asc_cdt_sec_acctg of BIG_DMPREC to Long_string.
	Move Long_string to CDT_SEC_ACCTG_FLG of RGW_MSG_CR_DMPREC.
	Move Sec_amt of Asc_cdt_sec_acctg of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to CDT_SEC_ACCTG_AMT of RGW_MSG_CR_DMPREC.
	Move Cdt_sec_currency of BIG_DMPREC to Cdt_sec_acctg_cur of RGW_MSG_CR_DMPREC.
	Move Cdt_secondary_acctg of BIG_DMPREC to Cdt_sec_acctg of RGW_MSG_CR_DMPREC.

	Move Ter_flg of Asc_cdt_ter_acctg of BIG_DMPREC to Long_string.
	Move Long_string to CDT_TER_ACCTG_FLG of RGW_MSG_CR_DMPREC.
	Move Ter_amt of Asc_cdt_ter_acctg of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to CDT_TER_ACCTG_AMT of RGW_MSG_CR_DMPREC.
	Move Cdt_tertiary_acctg of BIG_DMPREC to Cdt_ter_acctg of RGW_MSG_CR_DMPREC.
	Move Asc_cdt_amount of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to CDT_AMOUNT of RGW_MSG_CR_DMPREC.

	Move CDT_EXCH_RATE of BIG_DMPREC to STR18_EXCH.
	Move ASCII_EXCH to CDT_EXCH_RATE of RGW_MSG_CR_DMPREC.

	Move ZEROES to Record_Expired of RGW_MSG_CR_DMPREC.

* Field specific moves for differences in record definition

	move IDTYPE of CDT_ACCTG of BIG_DMPREC to CDT_ACCTG_IDTYPE of RGW_MSG_CR_DMPREC.
	move SLASH of CDT_ACCTG of BIG_DMPREC to CDT_ACCTG_SLASH of RGW_MSG_CR_DMPREC.
	move ACCOUNT of CDT_ACCTG of BIG_DMPREC to CDT_ACCTG_ACCOUNT of RGW_MSG_CR_DMPREC.

	Move Cdt_bilat_account of BIG_DMPREC to Cdt_bilat_acc of RGW_MSG_CR_DMPREC.

	Move C1 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C1 of RGW_MSG_CR_DMPREC.
	Move C2 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C2 of RGW_MSG_CR_DMPREC.
	Move C3 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C3 of RGW_MSG_CR_DMPREC.
	Move C4 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C4 of RGW_MSG_CR_DMPREC.
	Move C5 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C5 of RGW_MSG_CR_DMPREC.
	Move C6 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C6 of RGW_MSG_CR_DMPREC.
	Move C7 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C7 of RGW_MSG_CR_DMPREC.
	Move C8 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C8 of RGW_MSG_CR_DMPREC.
	Move C9 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C9 of RGW_MSG_CR_DMPREC.
	Move C10 of CDT_TRAN_CD_NAMES of BIG_DMPREC
		to CDT_TRAN_CD_NAMES_C10 of RGW_MSG_CR_DMPREC.

	Move Change_state of Cdt_value_date of msg_credit_set
		to Change_st of Cdt_value_date of RGW_MSG_CR_DMPREC.
	Move Adjuster of Cdt_value_date of msg_credit_set
		to Adjuster of Cdt_value_date of RGW_MSG_CR_DMPREC.
	Move Special of Cdt_value_date of msg_credit_set
		to Special of Cdt_value_date of RGW_MSG_CR_DMPREC.
	Move Change_state of cdt_book_date of msg_credit_set
		to Change_st of cdt_book_date of RGW_MSG_CR_DMPREC.
	Move Adjuster of cdt_book_date of msg_credit_set
		to Adjuster of cdt_book_date of RGW_MSG_CR_DMPREC.
	Move Special of cdt_book_date of msg_credit_set
		to Special of cdt_book_date of RGW_MSG_CR_DMPREC.

	Move Cdt_camefrom of msg_credit_set
		to Cdt_camefrom of RGW_MSG_CR_DMPREC.
	Move Ib1_camefrom of msg_credit_set
		to Ib1_camefrom of RGW_MSG_CR_DMPREC.
	Move Ibk_camefrom of msg_credit_set
		to Ibk_camefrom of RGW_MSG_CR_DMPREC.
	Move Bbk_camefrom of msg_credit_set
		to Bbk_camefrom of RGW_MSG_CR_DMPREC.
	Move Bnp_camefrom of msg_credit_set
		to Bnp_camefrom of RGW_MSG_CR_DMPREC.

%^ 107260 - extract special instructions

	Move Cdt_spc_inst1 of msg_credit_set
		to Cdt_spc_inst1 of RGW_MSG_CR_DMPREC.
	Move Cdt_spc_inst2 of msg_credit_set
		to Cdt_spc_inst2 of RGW_MSG_CR_DMPREC.
	Move Cdt_spc_inst3 of msg_credit_set
		to Cdt_spc_inst3 of RGW_MSG_CR_DMPREC.

%^ 110489 - move the adr_typ

	Move Country_code of cdt_adr_type of msg_credit_set
		to Cdt_adr_type_country of RGW_MSG_CR_DMPREC.

	Move Address_type of cdt_adr_type of msg_credit_set
		to Cdt_adr_type_typ of RGW_MSG_CR_DMPREC.

	Move Address_subtype of cdt_adr_type of msg_credit_set
		to Cdt_adr_type_subtyp of RGW_MSG_CR_DMPREC.

* Version 3.0
	Move Cdt_comm_cbl_flg of Flgs3 of msg_credit_set
		to Cdt_comm_cbl_flg of  RGW_MSG_CR_DMPREC.

Z801_CR_SETUP_END.
	exit.


Z802_DR_SETUP.

	Move Now_timestamp_ws to Now_timestamp of RGW_MSG_DR_DMPREC.

	Move Sec_flg of Asc_dbt_sec_acctg of BIG_DMPREC to Long_string.
	Move Long_string to DBT_SEC_ACCTG_FLG of RGW_MSG_DR_DMPREC.

	Move Sec_amt of Asc_dbt_sec_acctg of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to DBT_SEC_ACCTG_AMT of RGW_MSG_DR_DMPREC.
	Move Dbt_sec_currency of BIG_DMPREC to Dbt_sec_acctg_cur of RGW_MSG_DR_DMPREC.
	Move Dbt_secondary_acctg of BIG_DMPREC to Dbt_sec_acctg of RGW_MSG_DR_DMPREC.
	
	Move Ter_flg of Asc_dbt_ter_acctg of BIG_DMPREC to Long_string.
	Move Long_string to DBT_TER_ACCTG_FLG of RGW_MSG_DR_DMPREC.
	Move Ter_amt of Asc_dbt_ter_acctg of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to DBT_TER_ACCTG_AMT of RGW_MSG_DR_DMPREC.
	Move Dbt_ter_currency of BIG_DMPREC to Dbt_ter_acctg_cur of RGW_MSG_DR_DMPREC.
	Move IDBANK of DBT_TERTIARY_ACCTG of BIG_DMPREC to
	DBT_TER_ACCTG_IDBANK of RGW_MSG_DR_DMPREC.
	Move IDTYPE of DBT_TERTIARY_ACCTG of BIG_DMPREC to
	DBT_TER_ACCTG_IDTYPE of RGW_MSG_DR_DMPREC.
	Move IDKEY of DBT_TERTIARY_ACCTG of BIG_DMPREC to
	DBT_TER_ACCTG_IDKEY of RGW_MSG_DR_DMPREC.

	Move Asc_dbt_amount of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to DBT_AMOUNT of RGW_MSG_DR_DMPREC.

	Move DBT_EXCH_RATE of BIG_DMPREC to STR18_EXCH.
	Move ASCII_EXCH to DBT_EXCH_RATE of RGW_MSG_DR_DMPREC.

	Move ZEROES to Record_Expired of RGW_MSG_DR_DMPREC.

* Field specific moves for differences in record definition

	move IDTYPE of DBT_ACCTG of BIG_DMPREC to DBT_ACCTG_IDTYPE of RGW_MSG_DR_DMPREC.
	move SLASH of DBT_ACCTG of BIG_DMPREC to DBT_ACCTG_SLASH of RGW_MSG_DR_DMPREC.
	move ACCOUNT of DBT_ACCTG of BIG_DMPREC to DBT_ACCTG_ACCOUNT of RGW_MSG_DR_DMPREC.

	Move Dbt_bilat_account of BIG_DMPREC to Dbt_bilat_acc of RGW_MSG_DR_DMPREC.

	Move C1 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C1 of RGW_MSG_DR_DMPREC.
	Move C2 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C2 of RGW_MSG_DR_DMPREC.
	Move C3 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C3 of RGW_MSG_DR_DMPREC.
	Move C4 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C4 of RGW_MSG_DR_DMPREC.
	Move C5 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C5 of RGW_MSG_DR_DMPREC.
	Move C6 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C6 of RGW_MSG_DR_DMPREC.
	Move C7 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C7 of RGW_MSG_DR_DMPREC.
	Move C8 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C8 of RGW_MSG_DR_DMPREC.
	Move C9 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C9 of RGW_MSG_DR_DMPREC.
	Move C10 of DBT_TRAN_CD_NAMES of BIG_DMPREC
		to DBT_TRAN_CD_NAMES_C10 of RGW_MSG_DR_DMPREC.

	Move Change_state of Dbt_value_date of msg_debit_set
		to Change_st of Dbt_value_date of RGW_MSG_DR_DMPREC.
	Move Adjuster of Dbt_value_date of msg_debit_set
		to Adjuster of Dbt_value_date of RGW_MSG_DR_DMPREC.
	Move Special of Dbt_value_date of msg_debit_set
		to Special of Dbt_value_date of RGW_MSG_DR_DMPREC.
	Move Change_state of dbt_book_date of msg_debit_set
		to Change_st of dbt_book_date of RGW_MSG_DR_DMPREC.
	Move Adjuster of dbt_book_date of msg_debit_set
		to Adjuster of dbt_book_date of RGW_MSG_DR_DMPREC.
	Move Special of dbt_book_date of msg_debit_set
		to Special of dbt_book_date of RGW_MSG_DR_DMPREC.
	Move Dbt_camefrom of msg_debit_set
		to Dbt_camefrom of RGW_MSG_DR_DMPREC.
	Move Sbk_camefrom of msg_debit_set
		to Sbk_camefrom of RGW_MSG_DR_DMPREC.
	Move Obk_camefrom of msg_debit_set
		to Obk_camefrom of RGW_MSG_DR_DMPREC.
	Move Orp_camefrom of msg_debit_set
		to Orp_camefrom of RGW_MSG_DR_DMPREC.
	Move Ins_camefrom of msg_debit_set
		to Ins_camefrom of RGW_MSG_DR_DMPREC.
	Move Rca_camefrom of msg_debit_set
		to Rca_camefrom of RGW_MSG_DR_DMPREC.
	Move Dbt_prule_copy of msg_debit_set
		to Dbt_prule_copy of RGW_MSG_DR_DMPREC.
	Move Sbk_prule_copy of msg_debit_set
		to Sbk_prule_copy of RGW_MSG_DR_DMPREC.

%^ 107260 - extract special instructions

	Move Dbt_spc_inst1 of msg_debit_set
		to Dbt_spc_inst1 of RGW_MSG_DR_DMPREC.
	Move Dbt_spc_inst2 of msg_debit_set
		to Dbt_spc_inst2 of RGW_MSG_DR_DMPREC.
	Move Dbt_spc_inst3 of msg_debit_set
		to Dbt_spc_inst3 of RGW_MSG_DR_DMPREC.

%^ 110489 - move the adr_typ

	Move Country_code of dbt_adr_type of msg_debit_set
		to Dbt_adr_type_country of RGW_MSG_DR_DMPREC.

	Move Address_type of dbt_adr_type of msg_debit_set
		to Dbt_adr_type_typ of RGW_MSG_DR_DMPREC.

	Move Address_subtype of dbt_adr_type of msg_debit_set
		to Dbt_adr_type_subtyp of RGW_MSG_DR_DMPREC.

* Version 3.0
	Move Dbt_comm_cbl_flg of Flgs3 of msg_debit_set
		to Dbt_comm_cbl_flg of  RGW_MSG_DR_DMPREC.
	
Z802_DR_SETUP_END.
	exit.


Z807_RTEXT_SETUP.

	Move Now_timestamp_ws to Now_timestamp of RGW_RTEXT_DMPREC.

	Move SEQUENCE_NO of RTEXT_DMPREC to Long_string.
	Move Long_string to SEQUENCE_NO of RGW_RTEXT_DMPREC.

	Move ZEROES to Record_Expired of RGW_RTEXT_DMPREC.

Z807_RTEXT_SETUP_END.
	exit.

Z808_LIL_SETUP.

%^************************************************************
%^	PERIODX in RGW_LIL_DMPREC corresponds to NOW_TIMESTAMP
%^	in RGW_MSG_CR_DMPREC and RGW_MSG_DR_DMPREC
%^************************************************************
	Move Now_timestamp_ws to PERIODX of RGW_LIL_DMPREC
	If RCV_DATE of RGW_LIL_DMPREC = "******"
	then
	    Move spaces	to RCV_DATE of RGW_LIL_DMPREC
	End-if.
	If RCV_TIME of RGW_LIL_DMPREC = "***********" then
	    Move spaces	to RCV_TIME of RGW_LIL_DMPREC
	End-if.
%^
%^	Moves into ASCII_AMT_ND & ASCII_AMT are for debug purposes:
%^
	Move ASC_AMT of BIG_DMPREC to ASCII_AMT_ND.
	Move ASC_AMT of BIG_DMPREC to ASCII_AMT.
	Move BASE_AMOUNT of MSG_FTR_SET to AMT of RGW_LIL_DMPREC.

	Move ASC_EXCH_RATE of BIG_DMPREC to STR18_EXCH.
	Move ASCII_EXCH to EXCH_RATE of RGW_LIL_DMPREC.

	Move ORIG_EXCH_RATE of BIG_DMPREC to STR18_EXCH.
	Move ASCII_EXCH to ORIG_EXCH_RATE of RGW_LIL_DMPREC.

	Move ASC_FOR_AMT of BIG_DMPREC to ASCII_AMT_ND.
	Move ASC_FOR_AMT of BIG_DMPREC to ASCII_AMT.
	Move AMOUNT of MSG_FTR_SET to FOR_AMT of RGW_LIL_DMPREC.

	Move ASC_TOT_AMT of BIG_DMPREC to ASCII_AMT_ND.
	Move ASC_TOT_AMT of BIG_DMPREC to ASCII_AMT.
	Move TOTAL_AMOUNT of ACCTG of MSG_FTR_SET to TOT_AMT of RGW_LIL_DMPREC.

	Move ASC_OVER_DRAFT_AMT of BIG_DMPREC to ASCII_AMT_ND.
	Move ASC_OVER_DRAFT_AMT of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to OVER_DRAFT_AMT of RGW_LIL_DMPREC.

	Move AVAIL_BAL of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to AVAIL_BAL of RGW_LIL_DMPREC.

	Move LEDGER_BAL of BIG_DMPREC to STR18_AMT.
	Move ASCII_AMT to LEDGER_BAL of RGW_LIL_DMPREC.

	Move INTEREST_RATE of ACCTG of MSG_FTR_SET to INTEREST_RATE of RGW_LIL_DMPREC.

	If Over_timestamp of BIG_DMPREC not = spaces
	THEN
		Move Over_timestamp of BIG_DMPREC to X_timestamp_str
		%beg
		    Parse ^IN(X_timestamp_str) ^NOTRAP
			X_timestamp_ws, / ;
		%end
		Move X_timestamp_ws to Over_timstamp of RGW_LIL_DMPREC
	Else
		%beg X_timestamp_ws = null; %end
		Move X_timestamp_ws to Over_timstamp of RGW_LIL_DMPREC
	End-if.

	Move Stop_intercept_flg of msg_ftr_set
		to Ftr_stop_intcpt_flg of RGW_LIL_DMPREC.

	Move Change_state of due_date of msg_ftr_set
		to Change_st of due_date of RGW_LIL_DMPREC.
	Move Adjuster of due_date of msg_ftr_set
		to Adjuster of due_date of RGW_LIL_DMPREC.
	Move Special of due_date of msg_ftr_set
		to Special of due_date of RGW_LIL_DMPREC.
	Move Change_state of prime_send_date of msg_ftr_set
		to Change_st of prime_send_date of RGW_LIL_DMPREC.
	Move Adjuster of prime_send_date of msg_ftr_set
		to Adjuster of prime_send_date of RGW_LIL_DMPREC.
	Move Special of prime_send_date of msg_ftr_set
		to Special of prime_send_date of RGW_LIL_DMPREC.
	Move Change_state of second_send_date of msg_ftr_set
		to Change_st of second_send_date of RGW_LIL_DMPREC.
	Move Adjuster of second_send_date of msg_ftr_set
		to Adjuster of second_send_date of RGW_LIL_DMPREC.
	Move Special of second_send_date of msg_ftr_set
		to Special of second_send_date of RGW_LIL_DMPREC.
	Move msg_state of msg_ftr_set
		to msg_state of RGW_LIL_DMPREC.
	Move high_msg_state of msg_ftr_set
		to high_msg_state of RGW_LIL_DMPREC.
	Move Dbt_chrg_org of msg_ftr_set
		to Dbt_chrg_org of RGW_LIL_DMPREC.
	Move Cdt_chrg_org of msg_ftr_set
		to Cdt_chrg_org of RGW_LIL_DMPREC.
	Move Comm_chrg_org of msg_ftr_set
		to Comm_chrg_org of RGW_LIL_DMPREC.
	Move Cbl_chrg_org of msg_ftr_set
		to Cbl_chrg_org of RGW_LIL_DMPREC.
	Move Imposed_amount of Rptv_flgs of msg_ftr_set
		to Imposed_amount of RGW_LIL_DMPREC.
	Move Global_credit of Flgs5 of msg_ftr_set
		to Global_credit of RGW_LIL_DMPREC.
	Move Paymnt_funding_ind of Flgs5 of msg_ftr_set
		to Paymnt_funding_ind  of RGW_LIL_DMPREC.
	Move Paymnt_asynch_flg of Flgs5 of msg_ftr_set
		to Paymnt_asynch_flg of RGW_LIL_DMPREC.
	Move Paymnt_resp_time of Flgs3 of msg_ftr_set
		to Paymnt_resp_time of RGW_LIL_DMPREC.
	Move Mon_per_intercept_flg of Mon_flgs of msg_ftr_set
		to Mon_per_intercept_flg of RGW_LIL_DMPREC.
	Move Mon_per_log_flg of Mon_flgs of msg_ftr_set
		to Mon_per_log_flg of RGW_LIL_DMPREC.
	Move Smpl_Wrhs_Indicator of msg_ftr_set
		to Smpl_Wrhs_Indicator of RGW_LIL_DMPREC.
	Move Ent_ref_sys_vec of msg_ftr_set
		to Ent_ref_sys_vec of RGW_LIL_DMPREC.
	Move Wrhs_ref_sys_vec of msg_ftr_set
		to Wrhs_ref_sys_vec of RGW_LIL_DMPREC.
	Move Ref_call_vector of msg_ftr_set
		to Ref_call_vector of RGW_LIL_DMPREC.
	Move Smpl_pymt_format of msg_ftr_set
		to Smpl_pymt_format of RGW_LIL_DMPREC.
	Move Container_Rej_reason of msg_ftr_set
		to Container_Rej_reason of RGW_LIL_DMPREC.
	Move Unique_ref of msg_ftr_set
		to Unique_ref of RGW_LIL_DMPREC.

	Move ZEROES to Record_Expired of RGW_LIL_DMPREC.

Z808_LIL_SETUP_END.
	exit.

* Calculate the fields which do not exactly correspond from BIG_DMPREC to TRAIL_DMPREC.

Z809_TRAIL_SETUP.

	Move Country of Dbt_adr_type of BIG_DMPREC to Dbt_adr_type_country of TRAIL_DMPREC
	Move Country of Cdt_adr_type of BIG_DMPREC to Cdt_adr_type_country of TRAIL_DMPREC
	Move Trn_num of Trn_ref of BIG_DMPREC to Trn_num of TRAIL_DMPREC.
	Move Trn_date of Trn_ref of BIG_DMPREC to Trn_date of TRAIL_DMPREC.
	Move Value_date of BIG_DMPREC(3:6) to Value_date of TRAIL_DMPREC.
	Move Inst_date of BIG_DMPREC(3:6) to Inst_date of TRAIL_DMPREC.
*	Redefine conversion
	Move BASE_AMOUNT of MSG_FTR_SET to AMT of TRAIL_DMPREC.

	If Dbt_idtype of BIG_DMPREC not = spaces
	  or Dbt_id of BIG_DMPREC not = spaces
	then
	    Move Dbt_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Dbt_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move spaces to Overflow_ws of IDREC_ws
		Move IDREC_ws to Dbt_ident of TRAIL_DMPREC
	End-If.
	If Idtype of Dbt_acctg of LIL_DMPREC not = spaces
	  or Account of Dbt_acctg of LIL_DMPREC not = spaces
	then
	    Move Idtype of Dbt_acctg of LIL_DMPREC to IDtype_ws of IDREC_ws
		Move Account of Dbt_acctg of LIL_DMPREC	to ID_ws of IDREC_ws
		Move spaces to Overflow_ws of IDREC_ws
		Move IDREC_ws to Dbt_acctg_ident of TRAIL_DMPREC
	End-If.
	If Cdt_idtype of BIG_DMPREC not = spaces
	  or Cdt_id of BIG_DMPREC not = spaces
	then
		Move Cdt_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Cdt_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move spaces to Overflow_ws of IDREC_ws
		Move IDREC_ws to Cdt_ident of TRAIL_DMPREC
	End-If.
	If Idtype of Cdt_acctg of LIL_DMPREC not = spaces
	  or Account of Cdt_acctg of LIL_DMPREC not = spaces
	then
		Move Idtype of Cdt_acctg of LIL_DMPREC to IDtype_ws of IDREC_ws
		Move Account of Cdt_acctg of LIL_DMPREC	to ID_ws of IDREC_ws
		Move spaces to Overflow_ws of IDREC_ws
		Move IDREC_ws to Cdt_acctg_ident of TRAIL_DMPREC
	End-If.
*	Redefine conversion
	Move AMOUNT of MSG_FTR_SET to FOR_AMT of TRAIL_DMPREC.

	If BBK_idtype of BIG_DMPREC not = spaces
	  or BBK_id of BIG_DMPREC not = spaces
	then
		Move BBK_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move BBK_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to BBK_ident of TRAIL_DMPREC
	End-If.
	If Bnp_idtype of BIG_DMPREC not = spaces
	  or Bnp_id of BIG_DMPREC not = spaces
	then
		Move Bnp_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Bnp_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to BNP_ident of TRAIL_DMPREC
	End-If.
	If Ib1_Ib1_idtype of BIG_DMPREC not = spaces
	  or Ib1_Ib1_id of BIG_DMPREC not = spaces
	then
		Move Ib1_Ib1_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Ib1_Ib1_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to IB1_IB1_ident of TRAIL_DMPREC
	End-If.
	If Ibk_idtype of BIG_DMPREC not = spaces
	  or Ibk_id of BIG_DMPREC not = spaces
	then
		Move Ibk_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Ibk_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to IBK_ident of TRAIL_DMPREC
	End-If.
	If Obk_idtype of BIG_DMPREC not = spaces
	  or Obk_id of BIG_DMPREC not = spaces
	then
		Move Obk_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Obk_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to OBK_ident of TRAIL_DMPREC
	End-If.
	If Orp_idtype of BIG_DMPREC not = spaces
	  or Orp_id of BIG_DMPREC not = spaces then
		Move Orp_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Orp_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to ORP_ident of TRAIL_DMPREC
	End-If.
	If Sbk_idtype of BIG_DMPREC not = spaces
	  or Sbk_id of BIG_DMPREC not = spaces
	then
		Move Sbk_idtype of BIG_DMPREC to IDtype_ws of IDREC_ws
		Move Sbk_id of BIG_DMPREC to ID_ws of IDREC_ws
		Move IDREC_ws to SBK_ident 	of TRAIL_DMPREC
	End-If.

Z809_TRAIL_SETUP_END.
	exit.

*
* Extract the Processing Rules from the message credit, debit, sbk, and ftr sets.
*
Z900_PRULES.

	Set cdt_msg_ml in Getpr_level_asked to true.
	Set bad_is in Getpr_source_asked to true.

	Perform Z901_PRULE_RULES thru Z901_PRULE_RULES_end.

	Set dbt_msg_ml in Getpr_level_asked to true.
	Set bad_is in Getpr_source_asked to true.

	Perform Z901_PRULE_RULES thru Z901_PRULE_RULES_end.

	Set sbk_msg_ml in Getpr_level_asked to true.
	Set bad_is in Getpr_source_asked to true.

	Perform Z901_PRULE_RULES thru Z901_PRULE_RULES_end.

	Set msg_ml in Getpr_level_asked to true.
	Set bad_is in Getpr_source_asked to true.

	Perform Z901_PRULE_RULES thru Z901_PRULE_RULES_end.

********	Set bad_ml in Getpr_level_asked to true.
********	Set bad_is in Getpr_source_asked to true.

********	Perform Z901_PRULE_RULES thru Z901_PRULE_RULES_end.

Z900_PRULES_END.
	exit.


*
* Extract the Processing Rules for either an credit, debit, sbk, or ftr.
*
Z901_PRULE_RULES.

	Move 0 to Getpr_ordinal.
	Set Success_is in Getpr_rtn_cd to true.

	Perform until failure_is in Getpr_rtn_cd 
	   CALL "PRULE_MSG_READ_ALL_RULES" 
		using 	by reference	Getpr_level_asked,
			by reference 	Getpr_source_asked,
			by reference 	Getpr_ordinal,
			by reference 	Getpr_level_found,
			by reference 	Getpr_source_found,
			by reference 	Getpr_rule_type,
			by reference 	Getpr_rule_type_length,
			by reference 	Getpr_rule_subtype,
			by reference 	Getpr_rule_subtype_length,
			by reference 	Getpr_rule_source_id,
			by reference 	Getpr_rule_source_id_length,
			by reference 	Getpr_rule_descript,
			by reference 	Getpr_rule_descript_length,
			by reference 	Getpr_rule_name,
			by reference 	Getpr_rule_name_length,
			by reference 	Getpr_on_datetime,
			by reference 	Getpr_off_datetime,
	       returning Getpr_rtn_cd

	  If Success_is in Getpr_rtn_cd

	    If (msg_ml in 	Getpr_level_found OR
		sbk_msg_ml in 	Getpr_level_found OR
		dbt_msg_ml in 	Getpr_level_found OR
		cdt_msg_ml in 	Getpr_level_found)

		Initialize RGW_MSG_PR_DMPREC

* *** The following gets the TRN date, TRN time, and the timestamp.
		Move CORR RGW_MSG_CR_DMPREC to RGW_MSG_PR_DMPREC

		Move spaces			to Record_Expired of RGW_MSG_PR_DMPREC
		Move "N"			to Record_Updated of RGW_MSG_PR_DMPREC

	    	Add +1				to Prule_seq_num_ws
	    	Move Prule_seq_num_ws		to Pr_seq_num of RGW_MSG_PR_DMPREC
	    	Move getpr_rule_type 		to pr_type of RGW_MSG_PR_DMPREC
		Move Getpr_ordinal		to pr_ordinal of RGW_MSG_PR_DMPREC
		Move Getpr_source_found		to pr_source of RGW_MSG_PR_DMPREC
		Move Getpr_rule_subtype		to pr_subtype of RGW_MSG_PR_DMPREC
		Move Getpr_rule_source_id	to pr_source_id of RGW_MSG_PR_DMPREC
		Move Getpr_rule_descript	to pr_text of RGW_MSG_PR_DMPREC
		Move Getpr_rule_name		to pr_name of RGW_MSG_PR_DMPREC
		Move Getpr_on_datetime		to pr_effective_time of RGW_MSG_PR_DMPREC
		Move Getpr_off_datetime 	to pr_expiration_time of RGW_MSG_PR_DMPREC

* *** The following converts the message level oneof returned by the API into its DB level oneof equivalent.
		Evaluate TRUE

			When msg_ml in Getpr_level_found
				Set msg_is in pr_level of RGW_MSG_PR_DMPREC to True

			When sbk_msg_ml in Getpr_level_found
				Set sbk_is in pr_level of RGW_MSG_PR_DMPREC to True

			When dbt_msg_ml in Getpr_level_found
				Set dbt_party_is in pr_level of RGW_MSG_PR_DMPREC to True

			When cdt_msg_ml in Getpr_level_found
				Set cdt_party_is in pr_level of RGW_MSG_PR_DMPREC to True

		End-evaluate

	    	Write RGW_MSG_PR_DMPREC

		Perform Z902_READ_MSG_PR_EX thru Z902_READ_MSG_PR_EX_end
		Perform Z903_READ_MSG_PR_TEST thru Z903_READ_MSG_PR_TEST_end
	    End-if

	  End-if

	End-perform.


Z901_PRULE_RULES_end.
	exit.

*
* Extract the Processing Rules Execution parameters.
*
Z902_READ_MSG_PR_EX.

	Set Success_is in more_ex_parms to true.

	Move Spaces to Prev_prm_name_ws.

	Perform until failure_is in more_ex_parms
	   CALL "PRULE_MSG_READEX_PARM" 
		using 	by reference	Getex_prm_name
			by reference 	Getex_prm_name_length
			by reference 	Getex_prm_edit_type
			by reference 	Getex_prm_values_remain
			by reference 	Getex_prm_value
			by reference 	Getex_prm_value_length
	       returning more_ex_parms

  	  If Success_is in more_ex_parms

		If Getex_prm_name not = Prev_prm_name_ws

			Move Getex_prm_name to Prev_prm_name_ws
			Move 0 to values_seq_num_ws

			Initialize RGW_MSG_PR_PRM_DMPREC

		    	Move corr RGW_MSG_PR_DMPREC to RGW_MSG_PR_PRM_DMPREC
			Move Getex_prm_name to prparm_id of RGW_MSG_PR_PRM_DMPREC
			Move Getex_prm_edit_type to prparm_edit of RGW_MSG_PR_PRM_DMPREC
		
			write RGW_MSG_PR_PRM_DMPREC
		End-if

		Initialize RGW_MSG_PR_PVL_DMPREC

		add 1 to values_seq_num_ws
		move values_seq_num_ws		to value_seq_num of RGW_MSG_PR_PVL_DMPREC
		move CORR RGW_MSG_PR_PRM_DMPREC to RGW_MSG_PR_PVL_DMPREC
		move Getex_prm_value 		to parameter_values of RGW_MSG_PR_PVL_DMPREC

		write RGW_MSG_PR_PVL_DMPREC

	  End-if

	End-perform.

Z902_READ_MSG_PR_EX_end.
	exit.


*
* Extract the Processing Rules Match Criteria.
*
Z903_READ_MSG_PR_TEST.

	Set Success_is in more_tests to true.
	Move 0 to prtest_multi_values.

	Perform until failure_is in more_tests
	   CALL "PRULE_MSG_READ_RULE_TEST" 
		using 	by reference	Getex_prm_name
			by reference 	Getex_prm_name_length
			by reference 	prtest_name
			by reference 	prtest_name_length
			by reference 	prtest_cond_oneof
			by reference	prtest_op_oneof
			by reference	prtest_multi_values
			by reference	prtest_value
			by reference 	prtest_value_length
	       returning more_tests

  	  If Success_is in more_tests

		Initialize RGW_MSG_PR_MCH_DMPREC

		move CORR RGW_MSG_PR_DMPREC to RGW_MSG_PR_MCH_DMPREC

		move Getex_prm_name to prmatch_id of RGW_MSG_PR_MCH_DMPREC
		move prtest_cond_oneof to prmatch_cond of RGW_MSG_PR_MCH_DMPREC
		move prtest_op_oneof to prmatch_logical of RGW_MSG_PR_MCH_DMPREC
		move prtest_value to prmatch_value of RGW_MSG_PR_MCH_DMPREC

		write RGW_MSG_PR_MCH_DMPREC

	  End-if

	End-perform.

Z903_READ_MSG_PR_TEST_end.
	exit.



*
* Extract the Processing Rules for either an credit, debit, sbk, or ftr.
*
Z950_PRULE_SEQ.

	Move spaces				to Record_Expired of RGW_MSG_PR_DMPREC.
	Move "N"				to Record_Updated of RGW_MSG_PR_DMPREC.
	
	Perform until Seq_end_is in msg_proc_rule_cursor OR
		      Failure_is in msg_proc_rule_status
	
	    	Add +1					to Prule_seq_num_ws
	    	Move Prule_seq_num_ws			to Pr_seq_num of RGW_MSG_PR_DMPREC
	    	Move Corresponding msg_proc_rule	to RGW_MSG_PR_DMPREC

	    	Write RGW_MSG_PR_DMPREC

		Perform Z951_PRULE_MCH_SEQ thru Z951_PRULE_MCH_SEQ_end
		Perform Z952_PRULE_PRM_SEQ thru Z952_PRULE_PRM_SEQ_end
	    	
	    	%Beg Next: msg_proc_rule; %End
	
	End-perform.
	
	%Beg BREAK: msg_proc_rule; %End.

Z950_PRULE_SEQ_end.
	exit.
*
* Extract the Processing rules match data.
*
Z951_PRULE_MCH_SEQ.

	%Beg 
        Break: msg_match_seq;
	msg_proc_rule(NoTrap,
		.Pr_match_seq CONN: msg_match_seq ^FIRST); 
	msg_proc_rule(ETrap);
	%end.
	%ACE_IS msg_match_seq connected giving prule_conn_sts;

	Perform until Seq_end_is in msg_match_seq_cursor OR
		      Failure_is in msg_match_seq_status OR
		      Failure_is in prule_conn_sts

		Initialize				RGW_MSG_PR_MCH_DMPREC
		Move spaces				to Record_Expired of RGW_MSG_PR_MCH_DMPREC
		Move "N"				to Record_Updated of RGW_MSG_PR_MCH_DMPREC

		Move Corresponding RGW_MSG_PR_DMPREC	to RGW_MSG_PR_MCH_DMPREC
	    	Move Corresponding msg_match_seq	to RGW_MSG_PR_MCH_DMPREC
%^
%^ see if there are multiple (probably test-word) match values
%^ by probing the prmatch_val_list object in the match_seq
%^
		%Beg
		BREAK: match_value_seq(NOTRAP);
		msg_match_seq(NOTRAP);
		msg_match_seq.prmatch_val_list CONN: match_value_seq;
		msg_match_seq(ETRAP);
		match_value_seq(ETRAP);
		%End

		%ACE_IS match_value_seq connected giving match_val_seq_conn;

		If (Success_is in match_val_seq_conn)
		    %Beg
		    FIRST: match_value_seq;
		    %End
%^ the match value is in the field of the match seq
		    If (seq_end_is in match_value_seq_cursor)
	    	        Write RGW_MSG_PR_MCH_DMPREC
		    End-if
%^ the match values are in the match value sequence of the match seq
		    Perform UNTIL (seq_end_is in match_value_seq_cursor)
			Move Txt of match_value_seq to Prmatch_value of RGW_MSG_PR_MCH_DMPREC
	    	        Write RGW_MSG_PR_MCH_DMPREC
			%beg
			NEXT: match_value_seq;
			%end
		    End-Perform
		Else
	    	    Write RGW_MSG_PR_MCH_DMPREC
		End-if
	    
	    	%Beg Next: msg_match_seq; %End
	
	End-perform.
	
	%Beg 
	BREAK: msg_match_seq;
	%End.


Z951_PRULE_MCH_SEQ_END.
	exit.


*
* Extract the Processing Rules Parameter data.
*
Z952_PRULE_PRM_SEQ.

	%Beg 
        Break: msg_param_seq;
	msg_proc_rule(NoTrap,
		.pr_param_seq CONN: msg_param_seq ^FIRST); 
	msg_proc_rule(ETrap);
	%end.

	Perform until Seq_end_is in msg_param_seq_cursor OR
		      Failure_is in msg_param_seq_status

		Initialize				RGW_MSG_PR_PRM_DMPREC

		Move spaces				to Record_Expired of RGW_MSG_PR_PRM_DMPREC
		Move "N"				to Record_Updated of RGW_MSG_PR_PRM_DMPREC
	    
		Move Corresponding RGW_MSG_PR_DMPREC	to RGW_MSG_PR_PRM_DMPREC
	    	Move Corresponding msg_param_seq	to RGW_MSG_PR_PRM_DMPREC
	    	Write RGW_MSG_PR_PRM_DMPREC

		Perform Z953_PRULE_PVL_SEQ thru Z953_PRULE_PVL_SEQ_END
	    	
	    	%Beg Next: msg_param_seq; %End
	
	End-perform.
	
	%Beg 
	BREAK: msg_param_seq;
	%End.


Z952_PRULE_PRM_SEQ_end.
	exit.

*
* Extract the processing rules parameter values.
*
Z953_PRULE_PVL_SEQ.

	Move zeroes	to values_seq_num_ws.

	%Beg 
	msg_param_seq(NoTrap,
		   .prparm_value CONN: lcl_msg_value_seq ^FIRST); 
	msg_param_seq(ETrap);
	%end.
	
	Perform until Seq_end_is in lcl_msg_value_seq_cursor OR
		      Failure_is in lcl_msg_value_seq_status
	    
		Initialize				RGW_MSG_PR_PVL_DMPREC

		Move spaces				to Record_Expired of RGW_MSG_PR_PVL_DMPREC
		Move "N"				to Record_Updated of RGW_MSG_PR_PVL_DMPREC

	    	Add +1					to Values_seq_num_ws
	    	Move values_seq_num_ws			to Value_seq_num of RGW_MSG_PR_PVL_DMPREC

	    	Move Corresponding RGW_MSG_PR_PRM_DMPREC to RGW_MSG_PR_PVL_DMPREC
	    	Move Txt of lcl_msg_value_seq	        to Parameter_values of RGW_MSG_PR_PVL_DMPREC
	    	Write RGW_MSG_PR_PVL_DMPREC
	    	
	    	%Beg Next: lcl_msg_value_seq; %End
	
	End-perform.
	
	%Beg 
	BREAK: lcl_msg_value_seq;
	%End.

Z953_PRULE_PVL_SEQ_END.
	exit.
