%^ DEBITSIDE.COB source file.
*****************************************************************
*								*
* Written by Fred Isaacs, IntraNet, Inc.	  		*
*								*
* Copyright (c) 1995 - 2007					*
* by IntraNet, Inc., 1 Gateway Center, Newton, MA.		*
*								*
* This Software is confidential and proprietary to IntraNet   	*
* and it is protected by U.S. copyright law, other national   	*
* copyright laws, and international treaties. The Software may	*
* not be disclosed or reproduced in whole or in part in any   	*
* manner to any third party without the express prior written 	*
* consent of IntraNet, Inc.                                   	*
*                                                             	*
* This Software and its related Documentation are made        	*
* available under the terms of the IntraNet Software License  	*
* and may not be used, reproduced or disclosed in any manner  	*
* except as expressly authorized by the Software License. All 	*
* other uses are strictly prohibited.                         	*
*                                                             	*
* This Software and its related Documentation are proprietary 	*
* and confidential material of IntraNet, Inc.                 	*
*                                                             	*
*****************************************************************

%^******************************************************************************
%^
%^ Contains the following subroutine modules:
%^	DEBITSIDE_LOOKUP
%^	DEBITSIDE_SCREEN
%^	DEBIT_LOOK_ACCOUNT
%^	DEBIT_SCREEN_ACCOUNT
%^	DEBIT_REPET_STATE
%^                                                                           
%^ The associated DEBIT_REPETITIVE.COB source module should be maintained in
%^  parallel.  It contains the routines to clean up the debit side after an
%^  instance of a repetitive has been created.
%^
%^ The associated CREDITSIDE.COB source module should be maintained in
%^  parallel.  It contains the CREDITSIDE_* routines.
%^                                                                           
%^******************************************************************************



%^******************************************************************************
%^
%^      DEBITSIDE_LOOKUP
%^
%^******************************************************************************
%^
%^ Desired new features:
%^
%^	TODO - 
%^
%^
%^******************************************************************************
%^
%^ Description:
%^
%^	Contains message mapper lookup routines and supporting code to fill in
%^  and validate all message debit parties.  Any needed SI's are applied 
%^  during this process.
%^
%^ DEBITSIDE_LOOKUP routine.
%^
%^   Calling format:
%^	Call "Debitside_lookup" Using
%^		By Reference Nocheck_ambig
%^		By Reference Do_multibank
%^		By Reference Debit_currency
%^		By Reference Second_debit_id
%^		By Reference Second_debit_id_length
%^              By Reference Choose_by_currency
%^		By Reference Xbank_account_ok
%^		By Reference Notell_no_debit
%^		By Reference Nochange_bank
%^		By Reference Resume_SIs
%^		By Reference Message_amount
%^              By Reference Message_currency
%^		By Reference Debit_account
%^		By Reference Is_payment
%^		By Reference Account_type
%^		By Reference Is_repetitive_lookup
%^		By Reference Lock_dbt_party
%^		By Reference Special_fee_key
%^		By Refreence Party_rerun_ind
%^		By Reference Debit_completed
%^		By Reference Debit_internal_state
%^		By Reference Currency_found 
%^		By Reference Second_debit_matched
%^		By Reference Nothing_suspicious
%^		By Reference Msg_bank_changed
%^		By Reference Error_Memo_count
%^		By Reference Last_error_memo_text
%^		By Reference Last_error_memo_size
%^	    Giving Return_status_ws.
%^	Possible return status values are:
%^		SUCCESS
%^		FAILURE
%^
%^	  Called to fill out debit side of message, retrieving any address
%^    information for on-file parties and to identify a debit account.  We
%^    begin by processing the 
%^		 sending bank      (SBK)
%^		 originator's bank (OBK)
%^	    and	originating party  (ORP).
%^    The Nocheck_ambig and Do_multibank flag arguments control
%^    these address lookups and the debit party address lookup.  If
%^    Do_multibank is non-0, we will search all banks for the address.
%^    If it is not, we will search only the current bank.  If
%^    Nocheck_ambig is non-0, we will continue the address lookup
%^    within the bank for which we get our first hit to see if we get a
%^    second hit using the same key.
%^    If we can find the SBK, OBK, and ORP address information, we copy it 
%^    into the message, overwriting any name and address information parsed
%^    from the message; we also set up the rel_id and adr_ptr elements.  If
%^    it is ambiguous, we treat it as if we did not find it.
%^        We then look up the debit party address; the lookup is controlled
%^    by Do_multibank and Nocheck_ambig as described above.
%^    If there was no Dbt_typ and the Choose_by_currency argument is non-0 or 
%^    the Notell_no_debit argument is zero, an error memo will be generated and
%^    the routine will return FAILURE.  If the lookup failed we will return
%^    a NOF indicator.  If the lookup is ambiguous, we will return a Dbt_ovr 
%^    value of "?" (and if Choose_by_currency is non-0, an error memo ).  If 
%^    we were able to find the address in either the rel database or the aux 
%^    database, we will set up the debit party address information. 
%^	 If the address is ambiguous, we will try to resolve it by matching the
%^    message currency to accounts associated with the address.  To do this
%^    we need to find the accounts.  If the Xbank_account_ok argument is zero,
%^    we will restrict the currency match for debit accounts to debit accounts
%^    in the same bank as the candidate address.
%^	 If we were able to find the address and resolve any ambiguities,
%^    we then check it against the second_debit_id arguments if they are not 
%^    blank.  The Second_debit_matched argument will be returned 0 if the 
%^    second debit id does not match the address and non-0 if it does match.
%^       We have now resolved the debit party address as best we can.  
%^    If the address is on the rel file, Ent_d_adr_set will be connected to it.
%^	 DEBITSIDE_LOOKUP now proceeds to call its second half,
%^    DEBIT_LOOK_ACCOUNT, using a reserved value of the debit internal status
%^    to execute any debit AINs and to choose a debit account.  Debit AIN's 
%^    can have an advice-type trigger, though we do not think that this 
%^    actually occurs in practise.  If a debit AIN with an advice-type trigger
%^    is encountered which would otherwise execute, AIN execution is suspended
%^    with that debit party and a provisional debit account and currency are 
%^    selected using the current debit address. The value of the debit internal
%^    status returned to DEBITSIDE_LOOKUP (and passed back to its caller) 
%^    indicates whether AIN execution will need to be resumed on a second
%^    DEBIT_LOOK_ACCOUNT call. 
%^        The CREDITSIDE_LOOKUP routine will subsequently determine the advice
%^    method and call the DEBIT_LOOK_ACCOUNT routine to process any remaining
%^    debit AIN SIs and set up the debit parmoty address and account information.
%^	If any significant database anomalies were found in processing the
%^    debit party, the status is returned as FAILURE.  Otherwise it will be
%^    returned as SUCCESS.
%^      If none of the debitside parties were ambiguous and the debit party
%^    lookup succeeded if the debit party was specified, the Nothing_suspicious
%^    argument will be returned non-0 to indicate that this message may continue
%^    through automated payment processing.
%^    
%^ INPUT ARGUMENTS:
%^ explicit:
%^  Nocheck_ambig             Long    Non-0 if the (default) ambiguous key
%^	detection is to be suppressed.  The first item which matches an address
%^	identifier will be considered a match without checking for additional
%^	matches.
%^  Do_multibank 	      Long    Non-0 if the lookup is to be done
%^	across all banks beginning with the default bank.
%^  Debit_currency	      STR(3)  contains the debit currency.

%^	This is a mandatory field.
%^  Second_debit_id	      STR(132) contains the second debit id if one
%^	exists; if not it should be SPACES.
%^  Second_debit_id_length    Word contains the number of characters in the
%^	second debit id if one exists; if not it should be zero.
%^  Choose_by_currency                Long   If NON-0, message is a payment and
%^	both an unambiguous debit address and debit account are required.
%^  Xbank_account_ok	      boolean   Success if we are permitted to find a
%^	debit account in a different bank from the debit party.
%^  Notell_no_debit	      Long  Non-0 Suppresses error memo/notification if
%^	debit party itself is blank; this will presumably be detected and
%^	handled later by the caller.  (No debit party does not necessarily
%^	require notification and handling.  An ambiguous debit party always
%^	does.)
%^  Nochange_bank	      Long   Non-0 if a debit party bank which 
%^	disagrees with the Menu_bnk_union bank should not cause a bank
%^	context switch.  (The Msg_bank_changed flag will still be hoisted
%^	when appropriate even though no context switch will be done.)
%^  Resume_SIs	      	      Long     Non-0 if we are resuming SI processing
%^      after interrupting ourselves to handle an immediate action SI such
%^	as a COR. Pass-through to DEBIT_LOOK_ACCOUNT.
%^  Message_amount	      DEC(14.2)  contains the message amount.
%^	This is a mandatory field.
%^  Message_currency          STR(3)  contains the message currency.  This
%^      is the currency trigger for SI's.
%^  Debit_account	      ACC_ID_REC.DDF contains account ID if caller
%^  	has pre-determined it; else spaces.
%^  Is_payment		      Long   If NON-0, message is a payment and 
%^	both an unambiguous debit address and debit account are required.
%^  Account_type	      STR(1) Select an account of this type from
%^ 	the accounts associated with the debit party address.
%^  Is_repetitive_lookup      Long   Non-0 if this is a repetitive lookup,
%^	in which case we will not map special instructions nor copy the
%^	debit account's cnf_seq to the message.
%^  Lock_dbt_party            Long   Non-0 if the debit party is locked
%^      (such as by instantiation of a locked repetitive) and should not be
%^ 	changed by AINs, INTRTL tables, or anything else.
%^  Special_fee_key	      Str(1) Passed to Set_debit_account.  
%^				     SPACE for nothing special.
%^				     For now, "W" if the fees are to be waived.
%^ Party_rerun_ind	      Str(6)	indicate party changed by PD or Autotreive
%^					
%^
%^ implicit:
%^   Debit party:
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^   SBK:
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^   Menu_bnk_union.Id for current bank id.
%^
%^ OUTPUT ARGUMENTS:
%^explicit:
%^  Debit_completed           Long   Indicator of what state of completion the
%^	pre-creditside part of debitside-lookup achieved.
%^  Debit_internal_state      Long   Internal indicator of what state of 
%^	completion the pre-creditside part of debitside-lookup achieved.
%^  Currency_found            Str(3) is the debit currency found -- the
%^	explicit currency of the debit account, if any.  It will only be SPACES
%^	if no debit account was found.
%^  Second_debit_matched      Long  Returned as SUCCESS if second debit id was
%^	supplied and matched debit party; otherwise returned as FAILURE.
%^  Nothing_suspicious	      long   is returned 0 if any mapping errors
%^	were detected during debitside setup, otherwise 1.  For payments,
%^	mapping errors are caused by any ambiguous debitside parties, a
%^	NOF debit party, or a non-existent or ambiguous debit
%^	account.  For non-payments, mapping errors are caused by any
%^	ambiguous debitside party or a NOF (but not non-existent) debit
%^	party.  If this flag is returned 0, the message should be routed to
%^	repair instead of being passed through for automated payment
%^	processing.
%^  Msg_bank_changed	      long   is returned non-0 if the debit party was
%^      found in a different bank, requiring the message (context) bank to
%^      change.
%^  Error_Memo_count          Long   is the number of error or warning
%^	memos added to the message history during debitside lookup. 
%^	Informational memos indicating normal SI parsing and execution are
%^	NOT error or warning memos.
%^  Last_error_memo_text      Str(80) is the returned text of the last error
%^	or warning memo added to the message history during debitside lookup.
%^  Last_error_memo_size      Word    is the number of characters of returned
%^	text of the last error or warning memo added to the message history
%^	during debitside lookup.
%^  Return_status_ws is SUCCESS if no obviously bad or inconsistent
%^  rel or aux file data was read during debitside setup, otherwise FAILURE.
%^
%^Implicit:
%^   Message history
%^   Debit party:
%^	Ent_debit_set.Dbt_adr_set_ptr
%^	Ent_debit_set.Dbt_rel_id
%^	Ent_debit_set.Dbt_adr_bnk_id
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_account
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^	Ent_debit_set.Dbt_acc_class
%^	Ent_debit_set.Dbt_acc_parent_code
%^	Ent_debit_set.Dbt_acc_prod_codes
%^	Ent_debit_set.Dbt_adr_class
%^	Ent_debit_set.Dbt_adr_type
%^	Ent_debit_set.Dbt_concen_acc
%^	Ent_debit_set.Dbt_currency
%^	Ent_debit_set.Dbt_department
%^	Ent_debit_set.Dbt_recon_ref
%^	Ent_debit_set.Dbt_spc_inst1
%^	Ent_debit_set.Dbt_spc_inst2
%^	Ent_debit_set.Dbt_spc_inst3
%^	Ent_debit_set.Dbt_sys_of_rec
%^	Ent_debit_set.flgs.dbt_hold_flg
%^	Ent_debit_set.flgs.dbt_lim_flg  
%^	Ent_debit_set.flgs.dbt_ps_elig_flg
%^   SBK:
%^	Ent_debit_set.Sbk_adr_set_ptr
%^	Ent_debit_set.Sbk_adr_bnk_id
%^	Ent_debit_set.Sbk_rel_id
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk_adr_set_ptr
%^	Ent_debit_set.Obk_adr_bnk_id
%^	Ent_debit_set.Obk_rel_id
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp_adr_set_ptr
%^	Ent_debit_set.Orp_adr_bnk_id
%^	Ent_debit_set.Orp_rel_id
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^
%^
%^ Modification history:
%^
%^	Fred P. Isaacs	12-Apr-1995
%^		Initial version.
%^      Fred P. Isaacs  20-JUL-1995
%^		Now forces Dbt_adr_bnk_id to be Menu_bnk_union ID for NOF or
%^		ambiguous debit party and Cdt_adr_bnk_id to be same as
%^		Dbt_adr_bnk_id (or Menu_bnk_union ID for ENTRY if debit party
%^		bank is still blank).
%^      Fred P. Isaacs  31-JUL-1995
%^		Added Nochange_bank argument.
%^      Fred P. Isaacs  28-AUG-1995
%^              Tightened definition of full_parse for admins.  
%^		Tightened usage of Notell_no_debit to be payment only.
%^      Fred P. Isaacs  8-SEP-1995
%^              Tightened up full_parse setting logic and moved it to
%^		guaranteed exit path.
%^		Moved subsidiary party bank tag cleanup code to exit path.
%^      Fred P. Isaacs  13-SEP-1995
%^		Flag null DBT_IDTYPE as a NOF.
%^      Fred P. Isaacs  1-NOV-1995
%^		Suppress no debit account complaint for NOF debit party.
%^      Fred P. Isaacs  27-NOV-1995
%^		Correct nasty typo in account search.
%^      Fred P. Isaacs  28-NOV-1995
%^              Make sure that Side_acctyp_seq is released after use.
%^	Fred P. Isaacs  12-JAN-1996
%^	     Posted V 4.0 fixes to V4.1:
%^              1) Corrected over-enthusiastic setting of NOT fully parsed for
%^		   non-payment messages.
%^		2) Now properly sets title mode to let ACCOUNT_LOOKUP know 
%^		   target. 
%^	Fred P. Isaacs 5-FEB-1996
%^              Split off DEBIT_LOOK_ACCOUNT routine to do final debit party
%^		lookup and account resolution.  Sequence of events is now
%^		to call DEBITSIDE_LOOKUP to do subsidiary debitside parties
%^		and initial lookup of debit party; then to call
%^		CREDITSIDE_LOOKUP to completely determine all creditside 
%^		parties; CREDITSIDE_LOOKUP then calls DEBIT_LOOK_ACCOUNT to
%^              finish up the debit party SIs and set the debit address and
%^		debit account information.
%^      Fred P. Isaacs  6-FEB-1996
%^		Second idtype argument is now obsolete.  Second_dbt_id is now
%^		parsed just like Second_cdt_id and, like it, uses the AC,
%^		BC, CH, CP, and FW embedded idtypes. 
%^      Fred P. Isaacs  20-FEB-1996
%^              Correct IF nesting for no second idtype to make sure that
%^		address still gets set up.
%^	K. Callihan	9-APR-1996
%^		PER 13182 - BOA enhancement triggered by Rtn_dflt_acct intrtl
%^		When the flag is set and we look up debit party via an account
%^		that is designated not-on-file (*), then return default account
%^		(X) else error out.
%^      Fred P. Isaacs   10-APR-1996
%^              Pick up the Dbt_adr_class from the SBK if there is no debit
%^		party and the config flag says to do so.  Also use $ mode
%^		for lower debit parties.
%^      Fred P. Isaacs   19-APR-1996
%^              Make returned memo count get fudged to 2 from 1 if there are
%^		info memos since error memo may not be last.
%^		Insulate id in calls to ACCT_LOOKUP and make sure any changes
%^		made to fill out the id are kept.
%^	Fred P. Isaacs	17-MAY-1996
%^              Shortened error memos; made error /info memo write more robust.
%^	Fred P. Isaacs	22-MAY-1996
%^		Changed args for SI_FIRST_DEBIT.
%^		Changed DEBITSIDE_LOOKUP args to have separate amount and 
%^		   currency passed by caller.
%^              No longer gives error memo for NOF ABA coming in from FED.
%^	Fred P. Isaacs	28-MAY-1996
%^		No longer complains if it cannot find candidate account for
%^		on-file ABA from FED.
%^	Fred P. Isaacs	31-MAY-1996
%^		Make sure that Relget_msgcode is cleared before each lookup
%^		step so we don't confuse an AUX lookup hit with a complete miss
%^	John R. Phelan  19-JUL-1996
%^		Sender Confirmations - If sender confirmations are activated,
%^		copy any sending bank instructions with a delivery flag of "Y"
%^		into the delivery standing instruction sequence.
%^      John R. Phelan  31-JUL-1996
%^              Parse off any bank ID which may be present on the
%^              base account id when we set up the account seq search.
%^              Discard trailing address id if present.  Document new 4.2
%^		fields.
%^      John R. Phelan  08-AUG-1996
%^              Change Debitside_lookup and Debit_look_account not to return
%^              errors on the ORP and OBK party of adminisitrative messages.
%^              #19007
%^      John R. Phelan  13-AUG-1996
%^              Set Dbt_adr_bnk_id and Dbt_adr_set_ptr correctly.  #19333
%^      T.Carroll       20-AUG-1996
%^              Set priority_flag in the ent_ftr_set from the priority flag
%^		from the ent_d_adr_set whenever the debit party changes. #18571
%^      John R. Phelan  30-AUG-1996
%^              Set the NOF Dbt_account correctly.  #19175, #19199, #19313.
%^      John R. Phelan  27-SEP-1996
%^              Write an error memo for ambiguous debit parties.  #19851
%^      John R. Phelan  11-OCT-1996
%^              Allow head office insertion to take place if Sbk_name1
%^              came from Aux lookup and not from a mapper.  #21070
%^      John R. Phelan  31-OCT-1996
%^              Correct Bug in Debitside Lookups that Didn't Recognize 2nd IDs.
%^              #21872
%^      John R. Phelan  20-NOV-1996
%^              Clear out 2nd id field length.  It wasn't getting cleared out
%^              between messages, leading to bogus error messages.  #22559
%^      Fred P. Isaacs  22-NOV-1996
%^              Move code to set/reset the Dbt_adr_bnk_id and the bank of
%^              loc_info of Ent_ftr_set back here BEFORE creditside lookup
%^              happens so bank context of rest of message will be correct.
%^		#21277.  Migrated to 4.2 by John R. Phelan. 
%^      Fred P. Isaacs  22-NOV-1996
%^              Distinguishes between case of ambiguous debit party because
%^              we found more than one (Relget_msgcode = VMSG$_AMBIG_LOOKUP)
%^              and ambiguous because we only have a partial match
%^              (Relget_msgcode = VMSG$_PARTIAL_LOOKUP).  #21605.
%^		Migrated to 4.2 by John R. Phelan.
%^      Fred P. Isaacs  22-NOV-1996
%^              Applies all validity and paranoia checks to second ID's since
%^              some fed in from MERVA are very strange.   #21535, #22511.
%^		Migrated to 4.2 by John R. Phelan.
%^      John R. Phelan  30-DEC-1996
%^              Clear out debit party name and address when the address
%^              changes from on-file to not-on-file.  #21817
%^      John R. Phelan  23-JAN-1997
%^              Make sure address sets are always connected NOMOD.  #22302
%^      John R. Phelan   27-JAN-1997
%^              Add call(s) to SET_NOF_DBT_BNK_ID to determine not-on-file
%^              Dbt_adr_bnk_id based on various criteria.  #24397
%^      John R. Phelan   15-MAR-1997
%^              Change the search currency when the debit party is changed by
%^              an AIN.  #23745
%^      John R. Phelan   15-MAR-1997
%^              Clear out the Dbt_hold flag from the previous account whenever
%^              the Dbt_account field is cleared.  #24455
%^      John R. Phelan   15-MAR-1997
%^              Cross-currency changes.  #23745
%^      John R. Phelan   15-MAR-1997
%^              Make suggested changes to cross-currency messages.  #25675
%^  	M. Kuske    23-Mar-1997     SPR26497
%^  		Initial bug was that the routing flag was not being cleared when
%^  		the credit account number was changed.  Investigation indicated
%^  		much redundant code and that debitside should also be changed.
%^  		The changes included changes creditside.cob, debitside.cob, and
%^  		qual_output.cob.
%^
%^      John R. Phelan  19-MAY-1997    FUB 29039
%^              Turn on code to disambiguate address based on currency code
%^              in the baseline mode.
%^
%^      Fred Kelley    02-Jun-1997     FUB 28375
%^              Added Dsid_use_bank argument for GET_ACCOUNT_CURRENCY to
%^              determine when to use bank defaults and when not to.
%^
%^      Tom Carroll     12-JUN-1997    BOA 28336
%^              Force an AUX lookup of both a Chips id and ABA id as well as
%^              SWIFT when mapping an inbound chips originator.
%^
%^	A. Smith 28-Jun-1997		SPR #27131
%^		Use "P_ID" parameters in the debit party bank-union to replace
%^		hard-coded references to "CHP" as the source-code associated
%^		with the "P" idtype.  See "TODO(P_ID)" for places that would
%^		need refinement if a different meaning of the "P" idtype
%^		is supported in different banks. Currently the assumption is
%^		that all banks have the same "P_ID" record set up. There might
%^		also be a need for different clearing houses sharing the "P"
%^		idtype, for example one per currency.
%^
%^      J. Phelan 30-Jun-1997          SPR #30215
%^              When the Nochange_bank_ls argument is success, don't change the
%^              debit bank.
%^
%^	A. Smith 14-Aug-1997		SPR #32580
%^		Follow-up work on change of June 28.
%^
%^	Fred P. Isaacs  4-SEP-1997	SPR 28833
%^		Currency changes.
%^
%^	Fred P. Isaacs	24-NOV-1997	SPR 31248, 34811,36045
%^	Post accumulated V 4.2 changes:
%^          31248.  NOP here due to currency changes.
%^	    34811 Copied existing X940_TAKEOUT_GARBAGE paragraph from 
%^		CREDITSIDE and modified it to check form of debit party.
%^          35356.  Make lock flag not allow id substitution.
%^	    36045.  X940_TAKEOUT_GARBAGE paragraph was not allowing address id's
%^		to be appended to our account types (D, V, G, P, and F). 
%^
%^	Fred P. Isaacs  8-JAN-1998
%^	37782   Make call to clear account things and strip in confirmation for
%^	        NOF credit party unconditional.
%^      37793   Make sure that if second id is present, our internal version
%^		begins with a "/".
%^
%^      John R. Phelan  3-FEB-1998
%^      35648   Changed to call the new Validate_pid_adv_type subroutine to 
%^		determine the valid "P" source code, since this can now vary 
%^		depending on bank.  Changed to call the new Lookup_pid_rec 
%^		subroutine to lookup up "P" ID information, since this can 
%^		vary depending on bank, account suffix, currency, etc...  
%^		These changes are necessary for the Amex German Clearing 
%^		Interface.
%^
%^	T. Carroll	05-FEB-1998	SPR# 39052
%^	Corrected the setting of the priority from the address.  A lower
%^	number is a higher priority.
%^
%^	Fred P. Isaacs	18-FEB-1998	V 4.2 changes
%^ 	37303	Cleaned up exit on bad format debit id.
%^      38227   Likewise.
%^
%^	J. Carey	24-FEB-1998	SPR# 38651 
%^	Correct problem in paragraph X950_CHECK_ACCOUNT.
%^	If CHKDGT_EDIT changes the account number, it is desired to use
%^	the changed account number only if the parsing dictionary is not
%^	enabled.  The actual implementation was doing the opposite:  it was
%^	discarding the changed account number -- not using it -- only if the
%^	parsing dictionary was not enabled.
%^
%^      J. Phelan       24-Feb-1998     SPR27365
%^      Don't overlay the ORP, OBK and SBK party name and address if the mapper
%^      has already filled it in.
%^	
%^	Fred P. Isaacs	25-FEB-1998	REG 39796
%^	Cleaned up sloppy edit when I implemented 37303.
%^
%^	Fred P. Isaacs	2-MAR-1998	34811
%^	Make sure we get a clean exit on a debit id format failure.
%^
%^	Fred P. Isaacs  3-MAR-1998	40074
%^	Cleaned up setting of "completed" argument
%^
%^	Fred P. Isaacs 	6-MAR-1998	40119
%^	Fixed bug in HO pushdown status return -- now knows it did it okay.
%^
%^      Tom C. Carroll  25-MAR-1998     40407
%^      Placed a memo in the message history when set_debit_address sets
%^      the message priority.
%^
%^      John R. Phelan  13-APR-1998     41754
%^	Both Debitside_lookup and Debit_look_account were using the same
%^	field, Dsid_info_count.  Since Debitside_lookup can now call
%^	Debit_look_account directly, this was sometimes causing the 
%^	info memo count to be doubled.  Debitside_lookup has been changed
%^	to use a field Dsid_adr_info_count and Debit_look_account uses
%^	Dsid_acc_info_count so they don't interfere with each other.
%^	Dsid_dbt_party_error has also been split into two fields for the
%^	same reason.
%^
%^	Fred Isaacs	12-May-1998	42642
%^	Now properly eats trailing slash after account id (null address id).
%^
%^	J. Carey	04-June-1998	36259
%^	Changes to restrict cross-bank searches.
%^      Check INTRTL flag, NO_DBT_XBANK_SOURCES, which indicates sources
%^	for which a cross-bank search for the debit party is not permitted
%^	(regardless of what the caller requested).
%^	This flag will not affect the lookups for the lower debitside parties
%^	(ORP, OBK and SBK).
%^	Implementation:  The calling convention requires the caller (a mapper)
%^	to indicate whether cross-bank lookups are allowed (via parameter
%^	Do_multibank_ls).  Upon entry to DEBITSIDE_LOOKUP, this parameter is
%^	used to set local variable Dsid_multibank_ws.
%^      If the caller would permit multibank lookups, then we check the
%^	message source against the INTRTL flag to see if cross-bank searches
%^	on the debit party are permitted for this source.    
%^	The following local variables are established to control lookups:
%^	  - Dsid_multi_debit_party:  controls the lookup for debit party.
%^	  - Dsid_multibank_lower_party_ws: controls lookups for lower parties.
%^
%^      John Phelan      5-Aug-1998     45580
%^      In DEBITSIDE_LOOKUP, paragraph B280_DEBIT_PARTY, after ambiguous debit
%^      party cannot be resolved, we have written an error memo to thie
%^      message, set Debitside_look_status_ls to FAILURE, and Moved 1 to
%^      Dsid_dbt_adr_party_error, we should Move 0 to Dsid_rel_id.
%^
%^      John Phelan     14-Sep-1998   46264
%^      Consider the currency to be a match, if the search currency is the
%^      base currency of the bank, and the account currency is spaces.
%^
%^      J. Phelan       10-NOV-1998     #48905
%^      "If the primary credit id is an address id and the second credit id is
%^      an account id and if the second id account is found attached to the
%^      address found using the primary credit id, the primary credit id
%^      should be changed to the account specified in the second credit id.
%^      That will force use of the actual account as the credit account
%^      instead of simply selecting the default account as is done today." -
%^      from Fred Isaacs' problem writeup.  Same goes for the debitside lookups.
%^
%^	D. Ryan		10-May-1999	#15526 & 54097
%^	Add new argument to Acct_lookup (Acct_look_pend_del).
%^
%^	K. Hanlon	22-Jul-1999	#56233
%^	Only perform the search for a 2nd debit account in B300_SECOND_DEBIT
%^	in the account seq if 2nd id length not > 14 otherwise we will trap.
%^
%^	Fred P. Isaacs	14-Oct-1999	58119 for port
%^	PID suffix will be checked on suffixed PID against source;
%^	suffix for source will be appended onto unsuffixed PID.
%^
%^	Fred P. Isaacs	1-Nov-1999	57940 for port
%^	Initialize Currency_found_ls argument.
%^
%^	Fred Isaacs	11-JAN-2000     #60256
%^	Removed logic which disabled check digit account number change if
%^	Pd_enabled was "T".
%^
%^	Anna Caci	15-Sept-2000
%^	Code cleanup to resolve java translator issues.
%^
%^	H. Podany	19-Oct-2000
%^	Add new arguments returned from GET_PID_ADV_DATA.
%^
%^	Fred P. Isaacs	13-MAR-2001	70804
%^	Moved lower debit party lookup paragraphs from CREDIT_LOOKUP to be able
%^	to handle multiple id and id subtype syntax just like lower credit
%^	parties.  Added logic to X940_TAKEOUT_GARBAGE paragraph to squidge out
%^	a duplicate function indicator with a null id at the beginning of the
%^	ID.  (For example,  /AC7107896 becomes  /7107896, S/BCFREDUS33 becomes
%^	S/FREDUS33)
%^
%^	Fred P. Isaacs  5-APR-2001
%^ 73061 Make sure that /AC or " "/ in lower debitside party id is interpreted
%^	as a foreign account number, not one in our customer bank.  Also use
%^	Orp_id_overflow, Obk_id_overflow, and Sbk_id_overflow fields.
%^ 73165 Make sure that correct lower debit party idtype is passed to 
%^	SET_ORP_PARTY and SET_OBK_PARTY.
%^ 73373 Increase bulletproofing for bad input in lower debitside error checking
%^
%^	Fred P. Isaacs  18-AUG-2001  76204
%^	Make sure that address info for ORP and OBK parties is not used unless
%^	search for them succeeded so info is valid.
%^
%^ 	Ken Bjelke	16-Jan-2002	Spr #80869
%^	If "E" id type in c500_lower_debit is sent in incorrectly with only 1
%^	id present, don't trap, send back ambiguous.
%^
%^	Ken Bjelke	05-Feb-2002	Spr 81681
%^	Correct parameters for SET_ORP_PARTY and SET_OBK_PARTY (wrong party passed). 
%^
%^	Diana Pacelle	05-Apr-2002	Spr 76638
%^	Account number expansion.
%^
%^	Ken Bjelke	23-May-2002
%^	Add setting of ppp_res_country field for SBK
%^
%^	Ken Bjelke	23-Aug-2002
%^	Correct Profile connection. 
%^
%^	Ken Bjelke	30-Aug-2002 	Spr 88652
%^	Add Profile setting in SBK.
%^
%^	Ken Bjelke	17-Sep-2002	Spr 89318
%^	Correct the calling orders for PRULE_CHANGE_DEBIT and PRULE_UPDATE_PARTY
%^
%^ Ken Bjelke	01-Oct-2002
%^		Add parameter Party_rerun_ind. Will be used to allow DEBITSIDE LOOKUP
%^		to operate like debitside_screen for parties changed by PD or Autotreive.
%^
%^
%^ Ken Bjelke	6-Nov-2002	91650
%^		Corect setup for bnk profile connections
%^
%^ Fred P. Isaacs  11-NOV-2002  90950
%^		Changed Prule_party_fsect to make AUX prules work.
%^
%^
%^ Ken Bjelke	09-Dec-2002	Spr 93154	
%^		Add support for DBT IBAN party for mappers
%^
%^ M. Reyder	11-Dec-2002	spr 82812
%^		Add a new parameter dsid_iban_valid_flag_ws to call Validate_Iban
%^
%^ Ken Bjelke	12-Dec-2002	Spr 93151
%^		Add generation of PRM MTS$DBT_NATIVEACCOUNT when an IBAN is found
%^		on a debitside party.
%^
%^ Ken Bjelke	13-Dec-2002	
%^		make sure that xxx_adr_ptr_ok flags are set when required.
%^
%^ Ken Bjelke	03-Jan-2003	Spr 57385
%^		Do not use the Network GL when the dbt_idtype is an 'on us'
%^		idtype (D G F Or V). Also tighten up the dbt_adr_ptr_ok logic.
%^
%^ Ken Bjelke  08-Jan-2002	Spr 94295
%^		Use CHANNEL to get GL for NOF sources.
%^
%^
%^ Ken Bjelke 	05-Jan-2003	Spr 95527
%^		Inhibit all AIN's at level above address. 
%^		Spr 95318
%^		Correct erroneous Ambig screen presentation.
%^
%^ J. Curley	13-Feb-2003	SPR 95707
%^		Correct problem with finding accounts when SWIFT BIC and 
%^		Account_type_ls used - added check on Idtype as well as
%^		Currency.
%^
%^ S. Smith	24-Feb-2003	Spr 95329
%^		Change vmsg$_namemissing to ftrscr$_namemissing to avoid 
%^		video trap.
%^
%^
%^ Ken Bjelke	13-Mar-2003	96964
%^		Ensure that Counrty of residence is derived from BIC
%^		as a last resort for all debit side parties.
%^
%^ Ken Bjelke 	04-Apr-2004	98094
%^		Correct Clearing account processing. Was getting cleared
%^		in DEBIT_SCREEN_ACCOUNT.
%^
%^
%^ Ken Bjelke	11-Apr-2003	97811
%^		Allow lower parties using 4 character Extended id's to go
%^		STR rather than ambiguous. Maintains VAX functionality
%^		May require revisiting later.
%^
%^ J. Walsh	01-May-2003	SPR 98769
%^		In B240_SBK of debitside_lookup for mappers do not create pointer 
%^		from debit sbk to address if address is pending deletion.
%^
%^ C Czerwinski	12-JUN-2003	100426
%^		Increase dsid_iban_brnch_ws to vstr (10)
%^
%^ K Bjelke 	17-Jul-2003	102251 
%^		Correct the finding of the Debit account when a BIC provided.
%^
%^ J. Phelan	17-Jul-2003	100487
%^		Don't overlay xxx_res_country if already filled in.
%^
%^ Ken Bjelke 	14-Aug-2003	103339, 103528
%^		Correct calling of PRULE_CHANGE_DEBIT. AINs were not being found.
%^
%^ Ken Bjelke 	11-Sep-2003	102314
%^		Add check for MTS$CHANNEL_ACCOUNT PRM for RTGS/CLearing house
%^		incoming items.
%^
%^ Ken Bjelke 	25-Sep-2003 	104976
%^		Remove extra stripping of ID in TAKEOUT_GARBAGE, was not correct
%^		for the processing of the 4 character Extended ID types. Had
%^		Previously been removed in all other versions.
%^
%^ Cam Hansen   27-Oct-2003     59856
%^             made b280_debit_party_exit a separate paragraph that is now
%^             performed within b280_debit_party and commented out dead code
%^
%^ C. Crain	05-Jan-2004	97347
%^		Correct "E" id logic in paragraph X940_TAKEOUT_GARBAGE - id
%^		was incorrectly truncated two positions (bad subtype logic). 
%^
%^ Ken Bjelke 	05-Jan-2004	98053
%^ 		Replace the NO_DBT_XBANK_SOURCES table with DBT_XBANK_SRC_MT.
%^		New table has sources to Allow xbank sources and adds a incoming
%^		message qualifier.
%^
%^ Ken Bjelke  28-Jan-2004 	109459
%^	Allow for AIN SUB to work for own rel_id. This will allow a 
%^	DBTAIN to sub one of the address's accounts.
%^
%^ C. Crain	16-Feb-2004	PER 97347
%^	For Lower debit parties, replace "E" Idtype BCC with BIC based on
%^	LKUP_BIC_BY_BCC flag.
%^
%^ C. Crain	24-Feb-2004	SPR 109989
%^	Set no ambiguous check for in B240_SBK and C500_LOWER_DEBIT when "E"
%^	Idtype BCC and BCC Code Use configured (LKUP_BIC_BY_BCC).
%^
%^ Ken Bjelke 	28-Apr-2004	111825
%^		When a 4 character E ID is found, set dsid_ret_sat to failure,
%^		else calling paragraph will attempt to connect to REL. 
%^
%^ Ken Bjelke 	17-Jun-2004	113899
%^		ensure the lower party Address pointers are set. 
%^
%^ Ken Bjelke 	01-Oct-2004	113879
%^	Only set the lower parties xxx_adr_bnk_id if the rel_id not Non-0.
%^
%^ Fred P. Isaacs   14-Dec_2004	 119128
%^	Now replaces SPACE idtype with "D" and bban with stripped-out account
%^	number for iban or bban on us in debit party.
%^
%^ T. Welch	4-Jan-2006	mts 3.0 ui
%^	In debitside_lookup, debitside_screen, debitside_look_account,
%^	debitside_screen_account, arguments passed as boolean
%^	should be received as same (rather than as a long).
%^	Adjust all linkage and areas of code where arguments used.
%^
%^
%^ Ken Bjelke 	15-Mar-2006	129534
%^	Add call to GET_DBT_IDTYP_CHAN to populate the debit with a good IDtype
%^	based upon the source.
%^
%^ D. Boyajian	12-Feb-2007	SPR 136107
%^	In 3.0: .Declare ONEOF variable Dsid_val_iban_ret by a (revised)
%^		 Return_status_oneof.ddf rather than inline.
%^
%^ J. Carey	10-Feb-2009	CR6612 - Migrate from 2.0  --> J. Carey	 03-Dec-2008  CR5935
%^	Problem:  MTS sent SWF wire with a BEI in field 52A.
%^	That is prohibited.  The ID in field 52A cannot be a BEI.
%^	Explanation:  In certain cases, MTS is not identifying
%^	that a BIC is really a BEI.  Need to update the code
%^	that populates the Debit_set.Sbk_bei_flag.
%^	This code was not cognizant of all the Swf subtypes that
%^	indicate a BEI.  The SWIFT handbook for 2008 lists
%^	six Swift subtypes that that a BIC is really a BEI:
%^	    BEID, CORP, MCCO, SMDP, TESP, TRCO
%^
%^ J. Pfaff	12-Feb-2009	CR7795
%^      Add global fsect dsid_cust_fsect; remove local dsid_temp_party_id;
%^      add setting cst_party_usage.
%^
%^ C Czerwinski	26-Aug-2009	CR13142
%^	Move code missing in B300_get_acct_from_chan in Debit_screen_account
%^	to check for PRM
%^
%^ End Revision History
%^******************************************************************************

%^*****************************************************************************
%module DEBITSIDE_LOOKUP;
***************************************************************
*                                                             *
* This Software is confidential and proprietary to IntraNet   *
* and it is protected by U.S. copyright law, other national   *
* copyright laws, and international treaties. The Software may*
* not be disclosed or reproduced in whole or in part in any   *
* manner to any third party without the express prior written *
* consent of IntraNet, Inc.                                   *
*                                                             *
* This Software and its related Documentation are made        *
* available under the terms of the IntraNet Software License  *
* and may not be used, reproduced or disclosed in any manner  *
* except as expressly authorized by the Software License. All *
* other uses are strictly prohibited.                         *
*                                                             *
* This Software and its related Documentation are proprietary *
* and confidential material of IntraNet, Inc.                 *
*                                                             *
***************************************************************

%^*****************************************************************************
%^                                                                            *
%^ Paragraphs in this procedure are labeled as follows:                       *
%^                                                                            *
%^	A100_MAIN	     Subroutine entry point, dispatch, and return
%^      B200_ORP	     Looks up ORiginating Party.
%^      B220_OBK	     Looks up Originating BanK.
%^      B240_SBK	     Looks up Sending BanK.
%^      B280_DEBIT_PARTY     Looks up Debit Party and sets up message.
%^	B300_SECOND_DEBIT    Checks debit party against second id.
%^	B320_SETUP_2ND_ID    Sets up second id.
%^      C400_FIND_ADR_CUR    Disambiguate address based on currency match.
%^	C410_DBT_SWF_PARENT  Resolve NOF SWF branch by using parent.
%^	C440_COPY_ADDRESS    Copies address info into debit party.
%^	C480_CHECK_CURRENCY  Checks Side_acc_seq currency against message
%^	C500_LOWER_DEBIT     Looks up lower debit party.
%^	D100_BCC_LKUP_BIC    Replaces BCC with BIC based on config	
%^	D600_MATCH_IDS_TO_REL  Matches lower debit party id's to REL address
%^	D620_SWIFT_TO_ABA    Uses lower credit party SWIFT ID as index to XREF
%^      X900_ERROR_MEMO	     Writes an error or warning memo.
%^	X920_INFO_MEMO	     Writes an informational memo.
%^	X940_TAKEOUT_GARBAGE Checks a debit party id for garbage and format.
%^	X950_CHECK_ACCOUNT   Calls CHKDGT_EDIT to check an account ID.
%^	X960_SHUFFLE_IDS     Shuffles multiple ID's into standard order
%^                                                                            *
%^*****************************************************************************
%^ Macro definitions.
%^ %MAR
%^ .default	displacement,long
%^ %end
%^*****************************************************************************
%^ Subject definitions.
%def  		<AUX_FS>	%`SBJ_DD_PATH:AUX_FS_FSECT.DDL`	  	%end
%def  		<ENTFTR>	%`SBJ_DD_PATH:ENTFTR_FSECT.DDL`		%end
%def  		<ENT>		%`SBJ_DD_PATH:ENT_FSECT.DDL`		%end
%def  		<ENTREPT>	%`SBJ_DD_PATH:ENTREPT_FSECT.DDL`	%end
%^ %def  		<FTRSCRV>	%`SBJ_INCLUDE_PATH:FTRSCR.DEF`		%end
%def  		<ACE>		%`SBJ_DD_PATH:ACE_FSECT.DDL`		%end
%def  		<RELGET>	%`SBJ_DD_PATH:RELGET_FSECT.DDL`		%end
%def  		<PRULE_PARTY>	%`SBJ_DD_PATH:PRULE_PARTY_FSECT.DDL`	%end
%def 		<REL_PROFILE>	%`SBJ_DD_PATH:REL_PROFILE_FSECT.DDL`	%end
%def 		<REL_CHANNEL>	%`SBJ_DD_PATH:REL_CHANNEL_FSECT.DDL`	%end
%def 		<RELGET_SUBS>	%`SBJ_DD_PATH:RELGET_SUBS_FSECT.DDL`	%end		
%def            <DSID_CUST>     %`SBJ_DD_PATH:DSID_CUST_FSECT.DDL`      %end

%^
%^ Local storage subject definitions.
%def		<DSID_WS>	%^ local fsect
%^ *******  Subjects used by DEBITSIDE_LOOKUP *********

%^ Lookup status indicator
Dsid_ovr:	      	str(1) ;
Dsid_2nd_ovr:		str(1) ;

%^ Lookup returned status
Dsid_ret_stat: 		Boolean;
Dsid_ret2_stat: 	Boolean;
Dsid_conn_stat: 	Boolean;
Dsid_check_stat:	Boolean;
%^ Scratch id.
Dsid_idtype_ws:		str(1) ;
Dsid_id_ws:		vstr(64);
Dsid_return_key:        vstr(64) ;
Dsid_20charid_ws:	vstr(20) ;
Dsid_id_bank_ws:	vstr(3) ;
Dsid_acc_id_arg:	vstr(64) ;
Dsid_tmp_id_ws:		vstr(64);
Dsid_hold_idtype_ws:	Str(1);

%^ ID part of address specified by account.
Dsid_acc_id_ws:		vstr(64);

%^ Working id for debitside party.
Dsid_party_id:          vstr(132) ;
Dsid_party_idtype:      str(1) ;

%^ Used to determine new currency
Dsid_currency_ws:       str(3) ;
Dsid_hold_priority_ws:  str(1);

%^ Used to determine equivalent currency
Equiv_curr_ws:          str(3) ;
Equiv_ret_status:       Boolean ;
Save_dsid_curr_ws:      str(3) ;

%^ Character arguments for matching old lookup routine conventions.
Dsid_ambig_ws:		str(1) = " " ;
Dsid_multibank_ws:	str(1) = " " ;
Dsid_multi_debit_party: str(1) = " " ;
Dsid_multi_low_party:	str(1) = " " ;
Dsid_multibank_save_ws:	str(1) = " ";

%^ Address account seq for searches.
Dsid_acc_seq:		seq( %`SBJ_DD_PATH:REL_NAME_SEQ.DDF` );

%^ For finding default account for an address
Dsid_accdef_seq:	seq(	%`SBJ_DD_PATH:REL_NAME_SEQ.DDF` )
					Scan_key = Disp_default ;

%^ For finding account by type for an address
Dsid_acctyp_seq:	seq(	%`SBJ_DD_PATH:REL_NAME_SEQ.DDF`) 
				Scan_key = Rel_name_key.Idtype;

%^ Error memo text.
Dsid_err_memo:		vstr(132) ;

%^ Info memo text.
Dsid_info_memo:		vstr(132) ;

%^ Compose subject.
Dsid_compose:		COMPOSE(^NOTRAP) ;
Dsid_clip_compose:	COMPOSE(^NOTRAILING_BLANKS);
Dsid_parse:		PARSE(^NOTRAP) ;

%^ Returned rel id
Dsid_rel_id:		long ;

%^ Known debit account id
Dsid_know_acc_id:        rec(	%`SBJ_DD_PATH:ACC_ID_REC.DDF`);
%^ Holds duplicate account id.
Dsid_dupe_acc_ws:	rec(	%`SBJ_DD_PATH:ACC_ID_REC.DDF`);

%^ Scratch address id -- label of account sequence.
Dsid_scr_adr_id:        rec(	%`SBJ_DD_PATH:ADR_ID_REC.DDF`);

%^ Scratch address set
Dsid_adr_set:		set(    %`SBJ_DD_PATH:ADR_SET.DDF`);

%^ Save address set and idtype
Dsid_sav_adr_set:       set(    %`SBJ_DD_PATH:ADR_SET.DDF`);
Dsid_sav_idtype:        str(1);
Dsid_sav_ret_stat:      Boolean ;
Dsid_sav_relget_msgcode: Vstr(80);

%^ Temp bank union for currency checks.
Dsid_bnk_union:	set(	%`SBJ_DD_PATH:BNK_UNION.DDF`);

%^ Last qualified rel id.
Dsid_qualified_rel:	long ;
                                                  
%^ For scanning.
Dsid_next_status:	Boolean ;
Dsid_found_it:		Boolean ;
Dsid_found_account:	Boolean ;

%^ For account checks
Dsid_curr_okay:		Boolean ;
Dsid_type_okay:		Boolean ;
Dsid_acc_bank_ws:	str(3) ;
Dsid_bank_curr_ws:	str(3) ;
Dsid_acc_curr_ws:	str(3) ;
Vmsg_dat_notonfile_wc:	str  = "VMSG$_DAT_NOTONFILE";
Vmsg_missingdat_wc:	str  = "VMSG$_MISSINGDAT";
Vmsg_ambig_lookup_wc:   str  = "VMSG$_AMBIG_LOOKUP" ;
Vmsg_lookup_fail_wc:    str  = "VMSG$_LOOKUPFAIL" ;
Dsid_set_address:	Boolean ;
Dsid_set_account:	Boolean ;

%^ Preferred correspondent.
Dsid_preferred_corr:	rec( %`SBJ_DD_PATH:PARTY_REC.DDF`);
dsid_tmp_pref_corr:	rec( %`SBJ_DD_PATH:PARTY_REC.DDF`); %^ copy for rules processing

%^ Success in setting last preferred correspondent.
Dsid_push_error:	Boolean ;

%^ Number of SI types to match and return.
Dsid_number_SIs:	long = <0> ;

%^ Depth to which we have pushed credit/debit party on this cycle
Dsid_pushed_debit:	long;

%^ Count of ambiguous parties.
Dsid_ambig_parties:	word ;

%^ Specific party with problem
Dsid_debit_adr_erred:	Boolean;
Dsid_debit_acc_erred:	Boolean;
Dsid_sbk_party_erred:	Boolean;

%^ Count of parties with lookup problems.
Dsid_bogus_parties:	word ;

%^ Search mode for account lookup routine.
Dsid_account_mode:	long ;

%^ Temporary parking place for Ent_acc_set.
Dsid_acc_set:	set( %`SBJ_DD_PATH:ACC_SET.DDF`);

%^ VSTR to hold SI types for credit party AIN search
Dsid_SI_types:		vstr(90) ;

%^ Returned Si status from SI_FIRST_DEBIT
Dsid_si_error_ws:       Boolean;

%^ Suppress ADV debit SI trigger argument for SI_FIRST_DEBIT
Dsid_defer_adv_ws:	long ;

%^ Flag to indicate pushdown or overwrite for debit party.
Dsid_pushdown:		oneof(SUBSTITUTE, PUSHDOWN);

%^ Loop limiting counter to prevent an infinite overwrite loop
Dsid_push_count:	word ;

%^ Flag that party substitution was actually do to an SI
Dsid_didansi_ws:	Boolean ;

%^ Temp to hold second debit id for parsing
Dsid_2id_temp_ws:	vstr(132) ;

%^ Temps to hold the parsed reconstituted second debit id.
Dsid_2charg_idt:	str(1) ;
Dsid_2charg_id:		vstr(132) ;

%^ For parsing out embedded two-letter idtypes from second id.
Dsid_acchfwid:		vstr (124);
Dsid_oneof_acchfw:	word ;
Dsid_twoof_acchfw:	word ;
Dsid_2id_oneof:		word;

%^ Rationalized first part of 2-part second id.
Dsid_2aid_idt:		str(1) ;
Dsid_2aid_id:		vstr(132);
Dsid_2id_ws:		vstr(132);

%^ Rationalized second part of 2-part second id.
Dsid_2bid_idt:		str(1) ;
Dsid_2bid_id:		vstr(64) ;

%^ Temps to hold id being checked for second credit
Dsid_checkid_ws:	vstr(64) ;
Dsid_checkidt_ws:	str(1) ;

%^ Saved second debit id match status
Dsid_wascheck_ws:	long ;

%^ Argument for SI_FIRST_DEBIT
Dsid_previce_vstr:	vstr(8) ;

%^ Temporary idtype for lower debit party lookup
Dsid_temp_idtype:	str(1) ;

%^ Count for information messages
Dsid_adr_info_count:	long ;
Dsid_acc_info_count:	long ;

%^ Temp id for account lookup calls.
Dsid_lookup_temp:	vstr(64) ;

%^ Temp for writing out memo.
Dsid_temp_memo:		vstr(80) ;

%^ Keep track of whether we really need a debit account or not.
Dsid_musthave_acct:	Boolean ;

%^ For head office insertion
Dsid_sbk_name1_blank:   Boolean ;

%^ For second ID setup
Dsid_temp1_vstr:        vstr(132) ;
Dsid_temp2_vstr:        vstr(132) ;
Dsid_temp3_vstr:        vstr(132) ;
Dsid_temp4_vstr:        vstr(132) ;

%^ Internal states for debitside account processing
Dsid_internal:		oneof ( DSTATE_NONE, 	%^ Illegal value
				DSTATE_INIT,    %^ First time called
				DSTATE_DONE,    %^ Debitside all done
				DSTATE_HOLD,    %^ Debitside AIN hold
				DSTATE_BAD );   %^ Debitside serious error
Dsid_intern_word:	word ;
Dsid_intern_long:	long ;

%^ Status and "not suspect" flags and memo count for account lookup calls.
Dsid_acct_status:	Boolean ;
Dsid_acct_nosusp:	long ;
Dsid_acct_errmemos:	long ;
Dsid_acct_currency:	STR(3) ;
Dsid_adv_type:		STR(3) ;
Dsid_conn_ws:		long ;
Dsid_si_suspended:	long ;

dsid_account_type_ws:	Str(1);	%^ temp for account_type_ls

%^ Indicates that mapper passed on something it found indigestable.
Dsid_had_garbage:	Boolean ;

%^ Indicates that if garbage was found in a second id, it was in a DDA.
Dsid_bad_dda:           Boolean ;

%^ Used to hold extend subtypes and "/MM" introducer strings.
Dsid_party_extype:	str(2);
Dsid_party_extype2:	str(2);
Dsid_party_intro:	vstr(3);
Dsid_party_intro2:  	vstr(3);

Dsid_gcheck_oneof:	word ;        %^ Current idtype
Dsid_gcheck_twoof:	word ;        %^ Next idtype
Dsid_gcheck_adrof:	word ;	      %^ Appended address id	
Dsid_gcheck_states:	oneof (GCHT_NONE, GCHT_DDA, GCHT_SAV, GCHT_GL,
				GCHT_NOSTRO, GCHT_AC, GCHT_SWF, GCHT_CHUSER, 
				GCHT_FED, GCHT_CHIPS, GCHT_CLEAR, 
				GCHT_GARBAGE, GCHT_EXTEND, GCHT_NOMATCH ) ;
Dsid_party_bank_id:    vstr(3) ;     %^ Bank id
Dsid_gcheck_bank_id:    vstr(3) ;     %^ Bank id
Dsid_gcheck_id:		vstr(124) ;   %^ Next id
Dsid_gcheck_dda_id:	vstr(124) ;   %^ Saved so we can restore if PD active
Dsid_gcheck_processed:	vstr(124) ;   %^ ID is returned to caller here.
Dsid_gcheck_remain:	vstr(124) ;   %^ Remaining id string
Dsid_gcheck_remain2:	vstr(124) ;   %^ Other remaining id string
Dsid_gcheck_adr_id:	vstr(6) ;     %^ Holds slash and address id.
Dsid_temp_long:		long ;
Dsid_acc_idtype_ws:	str(1) ;
Dsid_loc_bank_change:	long ;
Dsid_acc_bank_change:	long ;

%^ Working flag to indicate if advice type is "CHP"-equivalent
Dsid_itsa_clearhouse:	Boolean;
Dsid_clrhs_adv:		Str(3);
Dsid_clrhs_swf_acc:	Str(2);
Dsid_clrhs_currency:	Str(3);
Dsid_clrhs_formatter:	Str(3);
Dsid_clrhs_min_length:	Long;
Dsid_clrhs_max_length:	Long;
Dsid_clrhs_mod_check:	Vstr(3);
Dsid_clrhs_ch_name:	Vstr(9);
Dsid_clrhs_suffix:	Vstr(3);
Dsid_clrhs_clr_sys:	Vstr(3);
Dsid_clrhs_destype:	Vstr(3);
Dsid_clrhs_dfincpy:	Vstr(3);
Dsid_clrhs_lqm_fnc:	Vstr(3);
Dsid_clrhs_clr_type:	Vstr(2);
Dsid_clrhs_clr_bic:	Vstr(11);
Dsid_clrhs_clr_gl:	Vstr(30);
Dsid_clrhs_clr_nstro:	Vstr(30);
Dsid_suffix_index:	long ;
Dsid_clrhs_pid_id:	Vstr(24);

%^ Subject variables for call to configuration routines.
Dsid_union_key_ws:   	rec (%`SBJ_DD_PATH:CFG_ID_REC.DDF`);
Dsid_item_key_ws:	vstr(25);
Dsid_item_type_ws:	vstr(16);
Dsid_item_data_ws:   	vstr(256);
Dsid_seq_ordinal_ws:	word;
Dsid_error_msg_ws:	vstr(80);
Dsid_match_key_ws:	vstr(80);
Dsid_length:		long;
Dsid_instring:		Str(80);
Dsid_intype_oneof:	word;
Dsid_debit_pend_del:	Boolean ;
Dsid_lkup_pend_del:	Str(1) ;
Dsid_210_ant_msg:	Boolean ;

%^ Lower debitside lookup temp IDs
Dsid_lc_aba:		vstr(64) ;
Dsid_lc_account:	vstr(64) ;
Dsid_lc_partic:		vstr(64) ;
Dsid_lc_swift:		vstr(64) ;
Dsid_lc_uid:		vstr(64) ;
Dsid_lc_extid:		vstr(64) ;
Dsid_lc_bogus:		vstr(64) ;
Dsid_lc_ansbak:         vstr(64) ;
Dsid_lc_dialdig:        vstr(64) ;
Dsid_lc_user:           vstr(64) ;
Dsid_lc_onrel:          vstr(64) ;
Dsid_lc_adridt:		vstr(1);
Dsid_lc_adrid:		vstr(64);
Dsid_lc_nowadr:		Boolean;

%^ Lower debitside lookup state flags.
%^     Values are "F" for found in id, "Y" for matched, "N" for conflict,
%^	"P" for cannot confirm, and SPACE for irrelevant.
Dsid_lc_flg_aba:	str(1) ;
Dsid_lc_flg_account:	str(1) ;
Dsid_lc_flg_partic:	str(1) ;
Dsid_lc_flg_swift:	str(1) ;
Dsid_lc_flg_uid:	str(1) ;
Dsid_lc_flg_extid:	str(1) ;
Dsid_lc_flg_ansbak:     str(1) ;
Dsid_lc_flg_dialdig:    str(1) ;
Dsid_lc_flg_user:       str(1) ;
Dsid_lc_flg_onrel:      str(1) ;
Dsid_lc_flg_bogus:	str(1) ;

%^ Lower debitside first lookup type.
Dsid_lc_first_idt:	str(1) ;

%^ Lower debitside party error message.
Dsid_lc_error:		word ;

%^ Lower debitside party no address flag
Dsid_lc_noaddress:	Boolean ;

Dsid_find_adr_idtype:	str(1);
Dsid_find_adr_id:    	vstr(64);

%^ For doing ABA lookups.
Dsid_aux_set:		set( %`SBJ_DD_PATH:AUX_DB_SET.DDF`);
Dsid_aux_index:		que( %`SBJ_DD_PATH:REL_NAME_INDEX.DDF`);
Dsid_temp_stat:		boolean;

%^ Temporaries for shuffling pieces of ids.
Dsid_shuffle_alt:	vstr(64) ;
Dsid_shuffle_part1:	vstr(64) ;
Dsid_shuffle_part2:	vstr(64) ;
Dsid_shuffle_acct:	vstr(34) ;
Dsid_shuffle_type:	str(2) ;
Dsid_shuffle_oneof:	word ;
Dsid_shuffle_parens:	vstr(64) ;
Dsid_shuffle_idt:	Str(1) ;
Dsid_gcheck_actodd:	Boolean ;	 %^ SUCCESS map /AC to "D", else to " ".
Dsid_had_ac:		Boolean;
Dsid_ftrscr_wf:		Boolean;
Dsid_disp_only_wf:	Boolean;
%^ to detect MENU command issued from ambiguous screens (REL_GET)
Dsid_next_function:	rec(%`SBJ_DD_PATH:MENU_FUNCTION_ID_REC.DDF`);



%^ Parameter/Rule processing stuff
dsid_prm_name:			vstr(40);
dsid_pr_level:		oneof(%` SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
dsid_pr_msglevel:	oneof(%` SBJ_DD_PATH:PRULE_MSGLEVEL_ONEOF.DDF`);
dsid_pr_source:		oneof(%` SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
dsid_prm_edit_type:		oneof(%` SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
dsid_prm_values_remain:		long;
dsid_prm_value:			Str(80);
dsid_pr_ret_stat:		Boolean;
dsid_upd_level:		oneof(%` SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);

dsid_pr_type_ws:	vstr(80);
dsid_pr_ordinal_ws:	long;
dsid_pr_subtype_ws:	vstr(80);
dsid_pr_memo:	vstr(%`%ACE$_MSG_STR_SIZE`);
dsid_pr_stat_ws:	boolean;

lcl_scan_stat_ws:	boolean;
lcl_param_name_ws:	vstr(80);
lcl_param_value_ws:	vstr(80);
Lcl_Param_type_ws:	Oneof(%`SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
lcl_prms_remaining_ws:	Long;


%^ GET_L items
dsid_prchan_mode:	Oneof(%`SBJ_DD_PATH:CHANNEL_IDTYPE_ONEOF.DDF`);
dsid_chan_ident_ws:	rec(%`SBJ_DD_PATH:ADR_ID_REC.DDF`);
dsid_got_prof_ws:	Boolean;
dsid_Got_channel_ws:	Boolean;
dsid_chan_imposed_acct:	Boolean;


%^ Items to allow for IBAN/BBAN processing
dsid_iban_bcode_found:		Boolean;
dsid_iban_found:		Boolean;
dsid_lookup_bban:		Boolean;
dsid_iban_cntry_ws:	str(2);
dsid_iban_bnk_ws:	vstr(10);
dsid_iban_brnch_ws:	vstr(10);
dsid_iban_curr_ws:	Str(3);
dsid_bnk_code_rec_ws:	rec(%`SBJ_DD_PATH:ADR_ID_REC.DDF`);
dsid_val_iban_ret:	ONEOF(%`SBJ_DD_PATH:RETURN_STATUS_ONEOF.DDF`);	%^ Spr 136107
dsid_bnk_code_bnk_ws:	str(3);
dsid_on_us_bnk_code:	Boolean;
%^ moved to fsect dsid_temp_party_id:	Vstr(132);	%^ holder for IBAN calls
dsid_iban_valid_flag_ws: Str(1);

dsid_orig_iban_ws:		vstr(132);
%^ Parameter processing 

dsid_prm_name_ws:		vstr(40);
dsid_prm_value_ws:		str(80);
dsid_prm_memo_ws:		vstr(80);
dsid_prm_level_wo:		oneof(%`SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
dsid_prm_source_wo:		oneof(%`SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
dsid_prm_edit_wo:		oneof(%`SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
dsid_prm_remaining_ws: 		long;
dsid_prm_status_wf:			boolean;
dsid_prm_mode_wf:		boolean;
dsid_prm_present_wf:		boolean;
dsid_prm_time_on_ws:		Time;
dsid_prm_time_off_ws:		Time;

Dsid_risk_country_ws:		str(2);
Dsid_country_code_ws:		str(2);
Dsid_res_country_ws:		str(2);
tmp_party_seq: 		seq( %`SBJ_DD_PATH:PRULE_SEQ.DDF`);

%^ PRULE account from channel checking
Mts_channel_account_wc:	Str = "MTS$CHANNEL_ACCOUNT";

dsid_src_ws:			Str(3);
dsid_mt_ws:			Vstr(4);

dsid_tmp_start_pos_ws:		long;
dsid_tmp_fin_pos_ws:		Long;
dsid_tmp_corr_len_ws:		Long;

dsid_lower_party_erred:		Boolean; %^ for lower errors in debitside_screen

%^ Lookup BIC by BCC 
Lkup_bic_by_bcc_ws:		str(1);
Bcc_lkup_status:		boolean;
Bcc_lkup_party:			str(3);
Bcc_now_bic:			boolean;
Dsid_ambig_save_ws:		str(1);

Dbt_screen_state:	Oneof(	Init_is,
			      	Orp_Lookup,
			      	Orp_index_conn,
			     	Orp_map,
			      	Obk_Lookup,
			      	Obk_index_conn,
			     	Obk_map,
			      	Sbk_Lookup,
				Sbk_Lookup2,
				Sbk_map,
			      	Sbk_index_conn1,
				Sbk_index_conn2,
			      	Ins_Lookup,
			      	Ins_index_conn,
			     	Ins_map,
			      	Dbt_Lookup1,
			      	Dbt_index_conn1,
			      	Dbt_Lookup2,
			      	Dbt_index_conn2,
			     	Dbt_map,
				Complete_is,
				Menu_is,
				Error_is);

Resolve_Ambiguous:		Boolean;

field_ws:			vstr(80);
Mnemonic_ws:			vstr(80);


Swf_aux_lookup_ws:		Boolean;
%End


%linkage
01  Nocheck_ambig_ls  	  	%long;
01  Do_multibank_ls 	  	%long;
01  Debit_currency_ls	  	%Str(3);
01  Second_dbt_id_ls	  	%Str(132);
01  Second_dbt_id_ls_length 	%Length;
01  Choose_by_currency_ls 	%Long;
01  Xbank_account_ok_ls	  	%boolean;
01  Notell_no_debit_ls    	%Long;
01  Nochange_bank_ls	  	%boolean;
01  Resume_SIs_ls	  	%Long;
01  Message_amount_ls	  	%Amount;
01  Message_currency_ls   	%Str(3);
01  Debit_account_ls	  	%Str(34);
01  Is_payment_ls	  	%boolean;
01  Account_type_ls  	  	%Str(1);
01  Is_rptv_lookup_ls	  	%boolean;
01  Lock_dbt_party_ls	  	%boolean;
01  Special_fee_key_ls	  	%Str(1);
01  Party_rerun_ind_ls		%Str(6);
01  Debit_completed_ls	  	%Long;
01  Internal_state_ls	  	%Long;
01  Currency_found_ls 	  	%Str(3);
01  Second_dbt_matched_ls 	%Long;
01  Nothing_suspicious_ls 	%boolean;
01  Msg_bank_changed_ls	  	%boolean;
01  Error_memo_count_ls	  	%Long;
01  Last_memo_ls 	  	%Str(80);
01  Last_memo_ls_length 	%Length;
01  Debitside_look_status_ls	%Boolean;

%Procedure using Nocheck_ambig_ls, Do_multibank_ls, Debit_currency_ls,
                Second_dbt_id_ls, Second_dbt_id_ls_length,
                Choose_by_currency_ls,
		Xbank_account_ok_ls, Notell_no_debit_ls, Nochange_bank_ls, 
		Resume_SIs_ls, Message_amount_ls, Message_currency_ls,
		Debit_account_ls, Is_payment_ls, Account_type_ls, 
		Is_rptv_lookup_ls, Lock_dbt_party_ls, Special_fee_key_ls,
		party_rerun_ind_ls,
		Debit_completed_ls, Internal_state_ls, Currency_found_ls, 
		Second_dbt_matched_ls, Nothing_suspicious_ls, 
		Msg_bank_changed_ls, Error_memo_count_ls, Last_memo_ls, 
		Last_memo_ls_length
	RETURNING  Debitside_look_status_ls.
%^*****************************************************************************

%^*****************************************************************************
A100_MAIN.
*  Initialize returned variables.
	Set Success_is in Debitside_look_status_ls to true.
	Move ZERO to Second_dbt_matched_ls.
	Set Failure_is in Nothing_suspicious_ls to true.
	Move ZERO to Error_memo_count_ls.
	Move ZERO to Dsid_adr_info_count.
	Move ZERO to Last_memo_ls_length.
	Move SPACES to Last_memo_ls.
	Move ZERO to Dsid_ambig_parties.
	Set Failure_is in Dsid_debit_adr_erred to TRUE.
	Set Failure_is in Dsid_debit_acc_erred to TRUE.
	Set Failure_is in Dsid_sbk_party_erred to TRUE.
    	Set Failure_is in Msg_bank_changed_ls to true.
	Move SPACES to Currency_found_ls.
	Set Failure_is in Dsid_gcheck_actodd to TRUE.
	Set Failure_is in Dsid_on_us_bnk_code to True.
	Move Spaces to dsid_orig_iban_ws.
        Set Debit_is in cst_party_usage to True.

	%^	Move Account_type_ls to dsid_account_type_ws. 
	
*  Set up local flags.
	If (Nocheck_ambig_ls NOT = 0 )
	    Move "T" to Dsid_ambig_ws 
	ELSE
	    Move SPACE to Dsid_ambig_ws 
	END-IF.
	If (Do_multibank_ls NOT = 0 )
	    Move "Y" to Dsid_multibank_ws 
	ELSE
	    Move SPACE to Dsid_multibank_ws 
	END-IF.

	Move Dsid_multibank_ws to Dsid_multi_debit_party
				  Dsid_multi_low_party.
	Set Failure_is in Dsid_debit_pend_del to TRUE.

%^ Get config flag for replacing lower debit party BCC Id with BIC
	Call "GCV_LKUP_BIC_BY_BCC" using
		by reference Lkup_bic_by_bcc_ws.

%^ 36259 - Check for possible restriction of multibank search on debit party.
%^	   A multibank search for the debit party will be disallowed if the
%^	   current source is found in the No_xbank_debit source table.
%^	   This flag will not affect the lookup for the lower parties on the
%^	   debitside.
%^ 	Replace with new table DBT_XBANK_SRC_MT
%^
	%beg
	    Dsid_union_key_ws(.Idname = "SOURCE_CODE_TABLES",
			      .Idprod = "MTS", 	   
		 	      .Idbank =  Menu_bnk_union.Bnk_id,    
		 	      .Idloc  = null,    	   
		 	      .Idcust = null);		  
	    Dsid_compose ^OUT(Dsid_item_key_ws)	"DBT_XBANK_SRC_MT:", /; 
	    Dsid_seq_ordinal_ws = <1>;
	    Dsid_match_key_ws = Ent_ftr_set.src_code;
	    dsid_item_type_ws = "VSTR(8)";
	%End
	Call "CFG_GET_ITEM" USING
	    		BY Reference Idname of Dsid_union_key_ws
			BY Reference Idprod of Dsid_union_key_ws
			BY Reference Idbank of Dsid_union_key_ws
			BY Reference Idloc of Dsid_union_key_ws
			BY Reference Idcust of Dsid_union_key_ws
			BY Reference Dsid_item_key_ws
			By Reference Dsid_seq_ordinal_ws
			By Reference Dsid_item_type_ws
	     		By Reference Dsid_item_data_ws
	     		By Reference Dsid_item_data_ws_length
			By Reference Dsid_error_msg_ws
        		By Reference Dsid_error_msg_ws_length
	RETURNING Dsid_ret_stat
 	If Success_is in Dsid_ret_stat  Then
		%^ We have a table, Clear the flag 
		%^ Now we need to examine the table and look for a MT match
		Move "I" to Dsid_multi_debit_party	%^ Set to Inhibit
		Call "CFG_GET_ITEM" USING
	    		BY Reference Idname of Dsid_union_key_ws
			BY Reference Idprod of Dsid_union_key_ws
			BY Reference Idbank of Dsid_union_key_ws
			BY Reference Idloc of Dsid_union_key_ws
			BY Reference Idcust of Dsid_union_key_ws
			BY Reference Dsid_item_key_ws
			By Reference Dsid_seq_ordinal_ws
			By Reference Dsid_item_type_ws
	     		By Reference Dsid_item_data_ws
	     		By Reference Dsid_item_data_ws_length
	     		By Reference Dsid_error_msg_ws
	     		By Reference Dsid_error_msg_ws_length
	   	RETURNING Dsid_ret_stat
		If Success_is in Dsid_ret_stat
		Then
		   Perform Until (Failure_is in Dsid_ret_stat) Or
			         (Dsid_multi_debit_party = "Y")
			%beg
				dsid_parse ^in(dsid_item_data_ws)
					dsid_src_ws,"|",dsid_mt_ws,/;
			%end
			If dsid_src_ws = Src_code of Ent_ftr_set Then
				If (dsid_mt_ws = "*")  Or
				   (dsid_mt_ws = Incoming_msgtype of Ent_ftr_set) Then
					Move "Y" TO Dsid_multi_debit_party
				end-if
			end-if
			If Dsid_multi_debit_party = "I" %^ we have not reset
			Then
				Add 1 to dsid_seq_ordinal_ws
				Call "CFG_GET_ITEM" USING
					BY Reference Idname of Dsid_union_key_ws
					BY Reference Idprod of Dsid_union_key_ws
					BY Reference Idbank of Dsid_union_key_ws
					BY Reference Idloc of Dsid_union_key_ws
					BY Reference Idcust of Dsid_union_key_ws
					BY Reference Dsid_item_key_ws
					By Reference Dsid_seq_ordinal_ws
					By Reference Dsid_item_type_ws
					By Reference Dsid_item_data_ws
					By Reference Dsid_item_data_ws_length
      					By Reference Dsid_error_msg_ws
      					By Reference Dsid_error_msg_ws_length
      				RETURNING Dsid_ret_stat
			end-if
		end-perform
	End-if.

 	Set Failure_is in Dsid_set_address to TRUE.

	IF (Resume_SIs_ls NOT = 0 )
	   Set Success_is in Dsid_set_address to TRUE
	   Go to A100_MAIN_EXIT
	END-IF.
%^ The Ho_insertion routine needs to know if Sbk_name1 was already mapped
        If Sbk_name1 of Ent_debit_set = SPACES
        THEN
            Set Success_is in Dsid_sbk_name1_blank to true
        ELSE
            Set Failure_is in Dsid_sbk_name1_blank to true
        END-IF.

        Move Debit_currency_ls to Dsid_currency_ws.


* Now let's process the parties from the bottom up, starting with the ORP.
*    The ORP, OBK, and SBK are not necessarily relevant.  We SHOULD have an
*      SBK, because that's who sent us the message.

	If (Orp_id of Orp of Ent_debit_set NOT = SPACE )
	    Perform B200_ORP through B200_ORP_END
	END-IF.

	If (Obk_id of Obk of Ent_debit_set NOT = SPACE )
	    Perform B220_OBK through B220_OBK_END
	END-IF.

	If (Sbk_id of Sbk of Ent_debit_set NOT = SPACE )
	    Perform B240_SBK through B240_SBK_END
	END-IF.

	IF (dbt_idtype of dbt_typ of Ent_debit_set = SPACE )
	   OR (dbt_id of dbt_typ of Ent_debit_set = SPACES )
	THEN
	    %Beg  Dsid_id_ws = Ent_debit_set.Dbt_typ.Dbt_id ;  %End
	    If (Dsid_id_ws_length NOT = 0 )
		%Beg
		Ent_debit_set.Dbt_typ.dbt_ovr = "*" ;
		%End
	    END-IF 
	    GO TO A100_MAIN_EXIT
	END-IF.

%^36259 - At this point, the lower debitside parties have been looked up,
%^        and we are about to lookup the debit party.  So we can overlay flag
%^	  Dsid_multibank_ws with the value of Dsid_multi_debit_party.
	Move Dsid_multibank_ws to Dsid_multibank_save_ws.
	Move Dsid_multi_debit_party to Dsid_multibank_ws.

        Perform B280_DEBIT_PARTY through B280_DEBIT_PARTY_END.

	If (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE) then
	    Perform B320_SETUP_2ND_ID through B320_SETUP_2ND_ID_END
	    If (Dsid_2aid_id_length NOT = 0 )
* We have an on-file debit party and a second debit id.	    
		%Beg 
%^  Hook up account sequence - we will need it.
                BREAK: Dsid_acc_seq ;
	        Ent_d_adr_set.account_seq CONN: Dsid_acc_seq (NOMOD) ;
		Dsid_checkid_ws = Dsid_2aid_id ;
		Dsid_checkidt_ws = Dsid_2aid_idt ;
                %End
	        Perform B300_SECOND_DEBIT through B300_SECOND_DEBIT_END
		If (Dsid_2bid_id_length NOT = 0 )
* We must also check the second second id.
		    Move Second_dbt_matched_ls to dsid_wascheck_ws
                    Move ZERO to Second_dbt_matched_ls
		    %Beg
		    Dsid_checkid_ws = Dsid_2bid_id ;
		    Dsid_checkidt_ws = Dsid_2bid_idt ;
		    %End
		    Perform B300_SECOND_DEBIT through  B300_SECOND_DEBIT_END
                    If (Second_dbt_matched_ls NOT = Dsid_wascheck_ws )
%^ Inconsistent results are a real no-no.
	   	        %Beg
		        Dsid_compose ^OUT(Dsid_err_memo)
			    "Inconsistent second debit ids ", Dsid_2aid_idt,
			    "/", Dsid_2aid_id, " and ", Dsid_2bid_idt, "/", 
			    Dsid_2bid_id, / ;
		        %End
	   		Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		    	Add 1 to Dsid_ambig_parties
			Set Success_is in Dsid_debit_adr_erred to TRUE
			GO TO A100_MAIN_EXIT
		    END-IF
		END-IF
		If (Second_dbt_matched_ls = 0 )
* Post an error memo to the message.
		    If (Dsid_2bid_id_length NOT = 0 )
	   	        %Beg
		        Dsid_compose ^OUT(Dsid_err_memo)
				"Second debit ids ", Dsid_2aid_idt, "/",
				Dsid_2aid_id, " and ", Dsid_2bid_idt, "/",  
				Dsid_2bid_id, " not in debit party ", 
			        Ent_debit_set.dbt_typ, / ;
		        %End
                    ELSE
	   	        %Beg
		        Dsid_compose ^OUT(Dsid_err_memo)
			        "Second debit id ", Dsid_2aid_idt, "/",
				Dsid_2aid_id, " not in debit party ", 
			        Ent_debit_set.dbt_typ, / ;
		        %End
                    END-IF
		    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		END-IF
	    END-IF
	END-IF.


A100_MAIN_EXIT.
* See if we can continue on to call DEBIT_LOOK_ACCOUNT
	%Beg
	Dsid_internal DSTATE_INIT ;
	Dsid_intern_word = Dsid_internal ;
	%End
	Move Dsid_intern_word to Internal_state_ls
	If (Success_is in Debitside_look_status_ls   )
	    Move Debit_currency_ls to Dsid_acct_currency
	    Call "DEBIT_LOOK_ACCOUNT" USING
		By Reference Dsid_acct_currency
		By Reference Debit_currency_ls
		By Reference Message_amount_ls
		By Reference Message_currency_ls
		By Reference Debit_account_ls
		By Reference Is_payment_ls
		By Reference Xbank_account_ok_ls
                 By Reference Account_type_ls
	%^By reference dsid_account_type_ws
		By Reference Resume_SIs_ls
		By Reference Is_rptv_lookup_ls
		By Reference Nochange_bank_ls
	 	By Reference Lock_dbt_party_ls
		By Reference Special_fee_key_ls
		By Reference Internal_state_ls
		By Reference Currency_found_ls
		By Reference Dsid_acct_nosusp
		By Reference Dsid_acc_bank_change
		By Reference Dsid_acct_errmemos
		By Reference Last_memo_ls
		By Reference Last_memo_ls_length
	      RETURNING Dsid_acct_status
	    If Dsid_acc_bank_change NOT = 0 
		Set success_is in Msg_bank_changed_ls to true
	    END-IF	
	    If Failure_is in Dsid_acct_status  
		Set Failure_is in Debitside_look_status_ls to true
	    END-IF
	    Add Dsid_acct_errmemos to Error_memo_count_ls
	ELSE
	    Move ZERO to Dsid_acct_errmemos
	    Move ZERO to Dsid_acct_nosusp
	END-IF.    	    	

* Here we format the subsidiary debitside parties.
	If ( Orp_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Orp_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Orp.Orp_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.orp.orp_id)
			Ent_debit_set.Orp_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

	If ( Obk_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Obk_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Obk.Obk_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.obk.obk_id)
			Ent_debit_set.Obk_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.
	If ( Sbk_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Sbk_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Sbk.Sbk_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.sbk.sbk_id)
			Ent_debit_set.Sbk_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

	If (Dsid_err_memo_length NOT = 0 )
            Move Dsid_err_memo_length to Last_memo_ls_length
            Move Dsid_err_memo(1:Dsid_err_memo_length) to Last_memo_ls
        END-IF.

	%Beg
	BREAK: Dsid_acc_seq ;
	%End.
* Info memo could obscure the error memo so make sure that they get
*     the multiple memo.
	Add Dsid_adr_info_count to Error_memo_count_ls.

%^ DEBITSIDE_LOOKUP and DEBITSIDE_LOOK_ACCOUNT should only return
%^ Nothing_suspicious_ls as 0 and/or their returned stati as NOT SUCCESS if the
%^ lookup error occurred on the SBK party address or the debit party address if
%^ there is no SBK party address and their Choose_by_currency_ls argument is 0.
%^ (Errors on the ORP or OBK debitside parties when the Choose_by_currency_ls
%^ argument is =0 should behave the same as CREDITSIDE_LOOKUP errors and
%^ Nothing_suspicious_ls should be returned set to 1 and the stati should
%^ be returned as SUCCESS.)
        If Choose_by_currency_ls = 0
            If Failure_is in Dsid_sbk_party_erred then
                If (Failure_is in Dsid_debit_adr_erred )
                   OR (Sbk of Ent_debit_set not = SPACES)
                   OR (Sbk_name1 of Ent_debit_set not = SPACES)
                   OR (Sbk_name2 of Ent_debit_set not = SPACES)
	        THEN
                    Set Success_is in Nothing_suspicious_ls to true
                    Set Success_is in Debitside_look_status_ls to true
		end-if
            end-if
	else 
	    If (Dsid_ambig_parties = 0 )
	       AND (Failure_is in Dsid_sbk_party_erred )
	       AND (Failure_is in Dsid_debit_adr_erred)
	       AND (Dsid_acct_nosusp = 1 )
	    THEN
* Debit party was completely processed.
	        Set Success_is in Nothing_suspicious_ls to true
                Set Success_is in Debitside_look_status_ls to true
	    END-IF
	END-IF.

	Move Internal_state_ls to Dsid_internal
        If (DSTATE_INIT in Dsid_internal )
	   AND (Dbt_ovr of Ent_debit_set NOT = SPACES )
	THEN
            Call "SET_NOF_DBT_ACCOUNT"
	    %Beg
	    Dsid_internal DSTATE_DONE ;
	    Dsid_intern_word = Dsid_internal ;
	    %End
	    Move Dsid_intern_word to Internal_state_ls
        END-IF.
	If (DSTATE_HOLD in Dsid_internal )
	THEN
	    Move ZERO to Debit_completed_ls
	ELSE
	    Move 1 to Debit_completed_ls
	END-IF.
                 
A100_MAIN_END.
        %EXIT PROGRAM.


B200_ORP.
*  Paragraph to do account lookup on the ORP; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	Set Failure_is in Dsid_found_it to TRUE.
	If ( (Orp_name1_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Orp_name1 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Orp_name2_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Orp_name2 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Orp_name3_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Orp_name3 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Orp_name4_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Orp_name4 of Ent_debit_set NOT = SPACES ) )
	THEN
	    Set Failure_is in Dsid_lc_noaddress to TRUE
	ELSE
	    Set Success_is in Dsid_lc_noaddress to TRUE
	END-IF.
	Set ORP in Relget_title_flag to TRUE.
	%beg  Dsid_party_idtype = Ent_debit_set.orp.orp_idtype ;  %End.
* Removed overflow test - obsolete
	%Beg
	    Dsid_clip_compose ^OUT(Dsid_party_id)
		Ent_debit_set.orp.orp_id, / ;
	%End.
	Set Failure_is in Dsid_gcheck_actodd to true
	Perform X940_TAKEOUT_GARBAGE through X940_TAKEOUT_GARBAGE_END.
	If (Failure_is in Dsid_had_garbage )
	    PERFORM X960_SHUFFLE_IDS through X960_SHUFFLE_IDS_END
	ELSE
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
	    	"Incorrect format in ORP ", Dsid_party_idtype, 
		Dsid_party_id, / ;         
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
	    GO TO B200_ORP_END
	END-IF.

	Perform C500_LOWER_DEBIT through C500_LOWER_DEBIT_END.

	If (Dsid_ovr = "?" )
* An ambiguous case -- kick it back to the operator.
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Not Str Thru: Ambiguous ORP party ", Dsid_ovr, 
			Ent_debit_set.Orp, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
	    GO TO B200_ORP_END
	END-IF.

	If (Dsid_ovr = SPACE )
	   OR ( (Dsid_ovr = "*" )
		AND (Success_is in Dsid_ret_stat )
		AND (Success_is in Dsid_lc_noaddress ) )
	THEN
	    Set Success_is in Dsid_found_it to TRUE
%^	    Lookup BIC using BCC
	    Move "ORP" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		GO TO B200_ORP_END
	    end-if
* Removed overflow logic - obsolete
	    If (Orp_id of Orp of Ent_debit_set NOT =
					Dsid_party_id(1:Dsid_party_id_length) )
	        %Beg  Ent_debit_set.Orp.Orp_id = Dsid_party_id ;  %End
	    END-IF
	
	    If Orp_idtype of Orp of Ent_debit_set NOT = Dsid_party_idtype 
	        %Beg  Ent_debit_set.Orp.orp_idtype = Dsid_party_idtype ;  %End
	    END-IF

            If (Dsid_ovr = SPACE )
	      AND (Rel_id of Relget_adr_set NOT = ZERO )
	    THEN
                %Beg
                Ent_debit_set (.Orp_rel_id  = Relget_adr_set.rel_id,
                               .Orp_adr_set_ptr POINT: Relget_adr_set,
			       .flgs3.Orp_adr_ptr_ok = "T",
			       .Orp_adr_bnk_id  = Relget_adr_set.bnk_id	);
                %End
	    END-IF
        END-IF.
	If Failure_is in Dsid_found_it
	    GO TO B200_ORP_END
	END-IF.
	Call "SET_ORP_PARTY" USING	
	    By reference Dsid_party_idtype
	    By reference Dsid_party_id
	    By reference Dsid_party_id_length
	    By reference Bank of Loc_info of Ent_ftr_set
	  Returning Dsid_ret_stat.

B200_ORP_END.


   EXIT.
B220_OBK.
*  Paragraph to do account lookup on the OBK; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	Set Failure_is in Dsid_found_it to TRUE.
	If ( (Obk_name1_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Obk_name1 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Obk_name2_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Obk_name2 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Obk_name3_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Obk_name3 of Ent_debit_set NOT = SPACES ) )
	   OR ( (Obk_name4_length of Ent_debit_set_lengths NOT = 0 ) 
             AND (Obk_name4 of Ent_debit_set NOT = SPACES ) )
	THEN
	    Set Failure_is in Dsid_lc_noaddress to TRUE
	ELSE
	    Set Success_is in Dsid_lc_noaddress to TRUE
	END-IF.
	Set OBK in Relget_title_flag to TRUE.
	%beg  Dsid_party_idtype = Ent_debit_set.obk.obk_idtype ;  %End.
* Removed overflow test - obsolete
	%Beg
	    Dsid_clip_compose ^OUT(Dsid_party_id)
		Ent_debit_set.obk.obk_id, / ;
	%End.
	Set Failure_is in Dsid_gcheck_actodd to true
	Perform X940_TAKEOUT_GARBAGE through X940_TAKEOUT_GARBAGE_END.
	If (Failure_is in Dsid_had_garbage )
	    PERFORM X960_SHUFFLE_IDS through X960_SHUFFLE_IDS_END
	ELSE
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
	    	"Incorrect format in OBK ", Dsid_party_idtype, 
		Dsid_party_id, / ;         
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
	    GO TO B220_OBK_END
	END-IF.

	Perform C500_LOWER_DEBIT through C500_LOWER_DEBIT_END.

	If (Dsid_ovr = "?" )
* An ambiguous case -- kick it back to the operator.
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Not Str Thru: Ambiguous OBK party ", Dsid_ovr, 
			Ent_debit_set.Obk, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
	    GO TO B220_OBK_END
	END-IF.

	If (Dsid_ovr = SPACE )
	   OR ( (Dsid_ovr = "*" )
		AND (Success_is in Dsid_ret_stat )
		AND (Success_is in Dsid_lc_noaddress ) )
	THEN
	    Set Success_is in Dsid_found_it to TRUE
%^	    Lookup BIC using BCC
	    Move "OBK" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		GO TO B220_OBK_END
	    end-if
* Removed overflow logic - obsolete
	    If (Obk_id of Obk of Ent_debit_set NOT =
					Dsid_party_id(1:Dsid_party_id_length) )
	        %Beg  Ent_debit_set.Obk.Obk_id = Dsid_party_id ;  %End
	    END-IF
	
	    If Obk_idtype of Obk of Ent_debit_set NOT = Dsid_party_idtype 
	        %Beg  Ent_debit_set.Obk.obk_idtype = Dsid_party_idtype ;  %End
	    END-IF

            If (Dsid_ovr = SPACE )
	      AND (Rel_id of Relget_adr_set NOT = ZERO )
	    THEN
                %Beg
                Ent_debit_set (.Obk_rel_id  = Relget_adr_set.rel_id,
                               .Obk_adr_set_ptr POINT: Relget_adr_set,
			       .flgs3.Obk_adr_ptr_ok = "T",
			       .Obk_adr_bnk_id  = Relget_adr_set.bnk_id );
                %End
	    END-IF
        END-IF.
	If Failure_is in Dsid_found_it
	    GO TO B220_OBK_END
	END-IF.
	Call "SET_OBK_PARTY" USING
	    By reference Dsid_party_idtype
	    By reference Dsid_party_id
	    By reference Dsid_party_id_length
	    By reference Bank of Loc_info of Ent_ftr_set
	  Returning Dsid_ret_stat.

B220_OBK_END.



   EXIT.
B240_SBK.
*  Paragraph to do account lookup on the SBK; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	Move ZERO to Relget_msgcode.
	Set SBK in Relget_title_flag to TRUE.
	Move SPACES to Dsid_lookup_temp.
	%Beg  Dsid_lookup_temp = Ent_debit_set.Sbk.Sbk_id ;  %End.
	Move SPACE to Dsid_lkup_pend_del.
	Move Dsid_ambig_ws to Dsid_ambig_save_ws.
%^	No ambig check for BCC Lookup of BIC
	If (Lkup_bic_by_bcc_ws = "D" or "B")
           AND Sbk_idtype of Sbk of Ent_debit_set = "E"
	    Move "T" to Dsid_ambig_ws
	End-if.

	Call "ACCT_LOOKUP" USING
	    by reference Sbk_idtype of Sbk of Ent_debit_set
	    by reference Dsid_lookup_temp
	    by reference Dsid_ovr
	    by reference Dsid_ambig_ws 
	    by reference Dsid_multi_low_party
	    by reference Debit_currency_ls
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	Move Dsid_ambig_save_ws to Dsid_ambig_ws.

	If (Dsid_ovr = SPACE )
	   OR ((Dsid_ovr = "*" )
		AND (Relget_msgcode = Vmsg_dat_notonfile_wc ))
%^	    Lookup BIC using BCC
	    Move "SBK" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
	        Set Success_is in Dsid_sbk_party_erred to TRUE
		GO TO B240_SBK_END
	    Else
		If Success_is in Bcc_now_bic
	    	    %Beg
		    Ent_debit_set.sbk.sbk_idtype = Relget_return_idtype;
		    Dsid_parse ^IN(Relget_return_key)
			    Ent_debit_set.sbk.sbk_id, ^SPACE, / ;
		    %End
		End-if
	    End-if
	END-IF.

	If (Dsid_ovr = SPACE )
	   AND (Relget_return_key NOT = SPACES )
	THEN
%^ Update id with what we actually found.
	    %Beg
	    Dsid_parse ^IN(Relget_return_key)
		    Ent_debit_set.sbk.sbk_id, ^SPACE, / ;
	    %End
	END-IF.
	If (Dsid_ovr = "?" )
* An ambiguous case -- kick it back to the operator.
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Not Str Thru: Ambiguous SBK party ", Dsid_ovr, 
			Ent_debit_set.SBK, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
            Set Success_is in Dsid_sbk_party_erred to TRUE
	    GO TO B240_SBK_END
	END-IF.

*  All we want here is name and address info that we can copy, so
*    an AUX DB hit is perfectly okay, too.
	IF (Dsid_ovr NOT = SPACE )
	    AND (Relget_msgcode NOT = Vmsg_dat_notonfile_wc )
	THEN
	    GO TO B240_SBK_CLEANUP
	END-IF.

	%^
	%^ Get the profile in if defined
	%^
	If (adr_profile_id_rec of relget_adr_set Not = Spaces) Then
	    %beg
		Ent_debit_set.Sbk_profile_id_rec =
			      relget_adr_set.adr_profile_id_rec;
	    %end
	end-if.

	If (Rel_id of Relget_adr_set NOT = ZERO ) and
		(Pending_del_date of Relget_adr_set = Zero)
	    %Beg
	    Ent_debit_set (.Sbk_rel_id 	= Relget_adr_set.rel_id,
			   .Sbk_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.Sbk_adr_ptr_ok = "T",
			   .Sbk_adr_bnk_id  = Relget_adr_set.bnk_id);

	    %End

	    If ( (Dbt_typ_length of Ent_debit_set_lengths NOT > 2 )
                 OR ( (Dbt_id of Dbt_typ of Ent_debit_set = SPACES )
		      AND (Dbt_idtype of Dbt_typ of Ent_debit_set = SPACE ) ) )
               AND ( (Dbt_name1_length of Ent_debit_set_lengths = 0 )
		     OR (Dbt_name1 of Ent_debit_set = SPACES ) )
               AND ( (Dbt_name2_length of Ent_debit_set_lengths = 0 )
		     OR (Dbt_name2 of Ent_debit_set = SPACES ) )
	       AND (Dbt_class_from_sbk of Menu_cfg = "T" )
	    THEN
%^ Take the Dbt_adr_class from the SBK since the debit party is blank
%^     and the config says we should do so.
	        %Beg
	        Ent_debit_set.Dbt_adr_class = Relget_adr_set.Adr_class;
	        %End
	    END-IF
	Else	%^ If rel_id = 0 
		%^ Check for AUX and set address ptr
		%^
		IF (Dsid_ovr NOT = SPACE )
	  	AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) THen
		     %beg	Ent_debit_set (.sbk_rel_id     =  <0>,
				               .sbk_adr_set_ptr POINT: relget_adr_set,
			                       .flgs3.sbk_adr_ptr_ok = "T") ;
	   	     %end
		end-if
	END-IF.


%^ Don't override the Sbk name and address if the mapper has already
%^ filled it in.
        If (Sbk_name1_length of Ent_debit_set_lengths not =
		zero and Sbk_name1 of Ent_debit_set not = spaces) or
           (Sbk_name2_length of Ent_debit_set_lengths not =
		zero and Sbk_name2 of Ent_debit_set not = spaces)
        THEN
            GO TO B240_SBK_CLEANUP
        END-IF.

%^      Do not increment database to initialize fields
        If (Adr_name_length of Relget_adr_set_lengths = ZERO )
           AND (Sbk_name1_length of Ent_debit_set_lengths NOT = ZERO )
        THEN
            %Beg  Ent_debit_set.Sbk_name1 = NULL;  %End
        END-IF.
        If (Adr1_length of Relget_adr_set_lengths = ZERO )
            AND (Sbk_name2_length of Ent_debit_set_lengths NOT = ZERO )
        THEN
            %Beg  Ent_debit_set.Sbk_name2 = NULL;  %End
        END-IF.
        If (Adr2_length of Relget_adr_set_lengths = ZERO )
            AND (Sbk_name3_length of Ent_debit_set_lengths NOT = ZERO )
        THEN
            %Beg  Ent_debit_set.Sbk_name3 = NULL;  %End
        END-IF.
        If (Adr3_length of Relget_adr_set_lengths = ZERO )
            AND (Sbk_name4_length of Ent_debit_set_lengths NOT = ZERO )
        THEN
            %Beg  Ent_debit_set.Sbk_name4 = NULL;  %End
        END-IF.
        %Beg
        dsid_compose  Relget_adr_set(
                                .Adr_name(^IF_NOTNULL(
                                    ^OUT(Ent_debit_set.Sbk_name1), ^_, /)),
                                .Adr1(^IF_NOTNULL(
                                    ^OUT(Ent_debit_set.Sbk_name2), ^_, /)),
                                .Adr2(^IF_NOTNULL(
                                    ^OUT(Ent_debit_set.Sbk_name3), ^_, /)),
                                .Adr3(^IF_NOTNULL(
                                    ^OUT(Ent_debit_set.Sbk_name4), ^_, /))
                                 );
        %End.

	If sbk_idtype of sbk of Ent_debit_set  = "S" Then
		If swf_subtype of relget_adr_set  = "BEID" or "CORP" or "MCCO" or "SMDP" or "TESP" or "TRCO" Then
			%beg  ent_debit_set.sbk_bei_flag = "Y"; %end
		else
			%beg  ent_debit_set.sbk_bei_flag = Null; %end
		end-if
	end-if.
*
* If there's a zip code, call subroutine to put it into address
*
	IF (Zip of Relget_adr_set NOT = SPACES ) THEN
	    Call "ZIPSUB" USING
		BY REFERENCE Sbk_name4 of Ent_debit_set
		BY REFERENCE Sbk_name4_length of Ent_debit_set_lengths
		BY REFERENCE Sbk_name3 of Ent_debit_set
		BY REFERENCE Sbk_name3_length of Ent_debit_set_lengths
		BY REFERENCE Zip OF Relget_adr_set,
		BY REFERENCE Line_flg_ws
	    If (Line_flg_ws = "3" ) 
		%Beg  Ent_debit_set.Sbk_name3 CHANGE ;  %End
	    ELSE
		%Beg  Ent_debit_set.Sbk_name4 CHANGE ;  %End
	    END-IF
	END-IF.


B240_SBK_CLEANUP.
%^
%^	Ensure that we fill in res_country if possible
%^
	%ACE_IS relget_adr_set Connected giving Dsid_ret_stat;.

	If sbk_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "SBK"
	       by reference sbk_idtype of Ent_debit_set
	       by reference sbk_id of Ent_debit_set
	       by reference sbk_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference sbk_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.sbk_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.
	
	%Beg
	BREAK: Prulepty_rule_seq(NOMOD);
	BREAK: Prulepty_party_adr_set(NOMOD);
	%End
	If Success_is in Dsid_ret_stat Then
		%beg Prulepty_source rel_is;
		     Prulepty_party_adr_ok Success_is;
	     	     Relget_adr_set.adr_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Relget_adr_set EQUATE: Prulepty_party_adr_set(NOMOD);
		%end
		If (Rel_id of Relget_adr_set = 0 )
		    %Beg  Prulepty_source aux_is;  %End
		end-if
	Else
		%beg Prulepty_source host_is;
		     Ent_debit_set.Sbk_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Prulepty_party_adr_ok Failure_is;
		%end
		Initialize Prulepty_party_adr_set
	end-if.

	%beg
	    Dsid_id_bank_ws = Ent_debit_set.Sbk_adr_bnk_id;
	    Prulepty_bank_prof_rec = NULL;
	%end
	If (Dsid_id_bank_ws = SPACES)
	    %Beg  Dsid_id_bank_ws = Ent_ftr_set.Loc_info.Bank;  %End
	end-if

	If (Dsid_id_bank_ws = Bnk_id of Menu_bnk_union)
		%beg
		     Prulepty_bank_prof_rec =
					     Menu_bnk_union.Bnk_profile_id_rec;
		%end
	else
		%Beg
		   BREAK: Dsid_bnk_union ;
		   SEARCH: Bnk_index 
			   (Key = Dsid_id_bank_ws );
		%end
 		If (Success_is in Bnk_index_status)
			%Beg Bnk_index CONN: Dsid_bnk_union(NOMOD); %end
		end-if
		%beg
  		   Prulepty_bank_prof_rec = Dsid_bnk_union.Bnk_profile_id_rec;
		%End
	end-if.
 
	Call "PRULE_CHANGE_SENDER" Returning Dsid_ret_stat.

	%beg dsid_upd_level sbk_is; %end
	Call "PRULE_UPDATE_PARTY" Using
		By Reference  dsid_upd_level
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.

	%^If Not(Success_is in Dsid_ret_stat) Then
	%^	Display "FAILURE IN PRULE_CHANGE_SENDER"
	%^	Display Dsid_pr_memo(1:dsid_pr_memo_length)
	%^Else
	%^	DISPLAY "Success in Update Sender"
	%^End-if.


B240_SBK_END.



   EXIT.
B280_DEBIT_PARTY.
* Paragraph to do actual lookup of debit party.
%^ Perform the debit party lookup.
	Move ZERO to Relget_msgcode.
	%Beg  BREAK: Ent_d_adr_set ;  %End.
	Move ZERO to Dsid_rel_id.


* Let's do a sanity check on the debit id.
	Set DBT in Relget_title_flag to TRUE.	%^ flag for takeout_garbage
	%Beg
	Dsid_party_id = Ent_debit_set.dbt_typ.dbt_id ;
	Dsid_party_idtype = Ent_debit_set.dbt_typ.dbt_idtype ;
	%End
	Set Success_is in Dsid_gcheck_actodd to true
	Perform X940_TAKEOUT_GARBAGE through X940_TAKEOUT_GARBAGE_END.
	If (Success_is in Dsid_had_garbage   )
	    Move "*" to Dsid_ovr
	    %Beg  Ent_debit_set.Dbt_typ.Dbt_ovr = "*" ;  %End
            %^ If there's an address pointer, hose it
            If  Dbt_rel_id of Ent_debit_set not = 0 then
                %Beg
                Ent_debit_set(  .Dbt_rel_id        = <0> ,
                                .DBt_adr_set_ptr DELETE,
				.flgs3.dbt_adr_ptr_ok = NULL) ;
                %End
		
            end-if
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
		"Incorrect format in debit party ", Ent_debit_set.dbt_typ, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Set Failure_is in Nothing_suspicious_ls to true
	    Set Success_is in Dsid_debit_adr_erred to TRUE
	    Set Failure_is in Debitside_look_status_ls to true
	    perform b280_debit_party_exit_rtn thru b280_debit_party_exit_rtn_end
            go to b280_debit_party_cleanup
	END-IF.
	If (Success_is in Dsid_on_us_bnk_code)
	THEN
	    %Beg
	    Dsid_party_idtype = "D";
	    Ent_debit_set.dbt_typ(.dbt_id = dsid_party_id,
				  .dbt_idtype = dsid_party_idtype);    
	    %End
	END-IF.
%^ OBSELETE
%^	If (Dbt_idtype of Dbt_typ of Ent_debit_set = "P")
%^ We think it came from a clearinghouse.  Check this assumption.
%^            Call "GET_PID_ADV_DATA" using
%^               Returning Dsid_itsa_clearhouse
%^ CALL is OBSELETE
%^	    If (Success_is in Dsid_itsa_clearhouse)
%^	       AND (Dsid_clrhs_suffix_length NOT = 0 )
%^	    THEN
%^		%Beg
%^		Dsid_temp1_vstr = NULL ;
%^		Dsid_temp2_vstr = NULL ;
%^		Dsid_temp3_vstr = NULL ;
%^		Dsid_parse ^IN(Dsid_party_id)
%^			^OPTION(Dsid_temp1_vstr(^STRING<3>), ":" ),
%^			Dsid_temp2_vstr, ^OPTION("/", Dsid_temp3_vstr),
%^			^SPACE, / ;
%^		%End
%^		Subtract Dsid_clrhs_suffix_length from Dsid_temp2_vstr_length
%^			giving Dsid_suffix_index
%^		Subtract 1 from Dsid_suffix_index
%^ Check to see if suffix is already on P id; if not, and P id is numeric,
%^   assume we have a naked P id and append the suffix.
%^		If (Dsid_temp2_vstr(Dsid_suffix_index:Dsid_clrhs_suffix_length)
%^			   NOT = Dsid_clrhs_suffix(1:Dsid_clrhs_suffix_length ))
%^		   AND (Dsid_temp2_vstr(1:Dsid_temp2_vstr_length) is NUMERIC )
%^		THEN
%^		    If (Dsid_temp1_vstr_length NOT = 0 )
%^		        %Beg
%^		        Dsid_clip_compose ^OUT(Dsid_party_id)
%^				Dsid_temp1_vstr, ":", Dsid_temp2_vstr,
%^				Dsid_clrhs_suffix ;  %End
%^		    ELSE	
%^		        %Beg
%^		        Dsid_clip_compose ^OUT(Dsid_party_id)
%^				Dsid_temp2_vstr, Dsid_clrhs_suffix ;  %End
%^		    END-IF
%^		    IF (Dsid_temp3_vstr_length NOT = 0 )
%^			%Beg
%^			Dsid_clip_compose  "/", Dsid_temp3_vstr, / ;  %End
%^		    ELSE
%^			%Beg
%^			Dsid_clip_compose  / ;  %End
%^		    END-IF
%^		    %Beg
%^		    Ent_debit_set.Dbt_typ.Dbt_id = Dsid_party_id;  %End
%^		END-IF
%^	    END-IF
%^	END-IF
	Set DBT in Relget_title_flag to TRUE.
	Move "F" to Dsid_lkup_pend_del
	Call "ACCT_LOOKUP" using
	    by reference Dsid_party_idtype
	    by reference Dsid_party_id
	    by reference Dsid_ovr
	    by reference Dsid_ambig_ws 
	    by reference Dsid_multi_debit_party
	    by reference Debit_currency_ls
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

        If Dsid_lkup_pend_del = "T"
	    Set Success_is in Dsid_debit_pend_del to TRUE
        End-if.

	%Beg  Ent_debit_set.dbt_typ.dbt_ovr = Dsid_ovr ;  %End
%^ Check for lookup failed or ambiguous and clear debit party relation id 
%^     and stuff.
	IF (Dsid_ovr NOT = SPACE )
	   AND (Failure_is in Dsid_debit_pend_del)
	THEN
%^ If the party was not found in the REL, and if there is not already a party 
%^   which will have to be checked for debit authority, try an implied debit
%^   authority of the sending 11 char ID over the 8 character ID.
	    If (Map_ho_insertion of Menu_cfg is = "D" OR "B")
	       AND (Dbt_idtype of Dbt_typ of Ent_debit_set = "S" )
               AND (Success_is in Dsid_sbk_name1_blank  )
	       AND (Sbk_idtype of SBK of Ent_debit_set =
                                      Dbt_idtype of Dbt_typ of Ent_debit_set )
	       AND (Sbk_id of SBK of Ent_debit_set =
                                      Dbt_id of Dbt_typ of Ent_debit_set )
		
	    THEN	
	        PERFORM C410_DBT_SWF_PARENT thru C410_DBT_SWF_PARENT_END
	    END-IF
	END-IF.
	If (Dsid_ovr = SPACE )
	    Move Rel_id of Relget_adr_set to Dsid_rel_id
	    If (Relget_return_key NOT = SPACES )
	        %Beg
	        Dsid_parse ^IN(Relget_return_key)
		    Ent_debit_set.dbt_typ.dbt_id, ^SPACE, / ;
	        %End
	    END-IF
	END-IF.

	IF (Dsid_ovr = "?" )
           AND (Relget_msgcode = vmsg_ambig_lookup_wc )
        THEN
	    Move ZERO to Dsid_qualified_rel
	    If (Debit_currency_ls NOT = SPACES )
	    THEN
*  Debit party is ambiguous, so let's see if currency disambiguates it.
	        PERFORM C400_FIND_ADR_CUR THRU C400_FIND_ADR_CUR_END
	    END-IF

	    If (Dsid_qualified_rel NOT = 0 )
* We have successfully disambiguated the debit address.	
		%ace_conn_root_q Rel_index ;
                %Beg
		BREAK: Relget_adr_set ;
		Rel_index ^SEARCH (Key = Dsid_qualified_rel);
		%end
		If (Success_is in Rel_index_status   )
		   AND (OBJECT_IS in Rel_index_cursor )
		   AND (ADDRESS_IS in Rel_type of Rel_index )
                THEN
		    %Beg
		    Rel_index CONN: Relget_adr_set(NOMOD) ;
		    Dsid_ovr = " " ;
		    Ent_debit_set.Dbt_typ.Dbt_ovr = Dsid_ovr ;
		    %End
		    Move Rel_id of Relget_adr_set to Dsid_rel_id
		END-IF
	    END-IF
	END-IF.

%^  If the party is still ambigous, write an error memo
        If Dsid_ovr = "?"
          then
            If  Dbt_rel_id of Ent_debit_set not = 0 then
                %Beg
                Ent_debit_set(  .Dbt_rel_id        = <0> ,
                                .Dbt_adr_set_ptr DELETE,
			        .flgs3.dbt_adr_ptr_ok = Null) ;
                %End
            end-if
            Move ZERO to Dsid_rel_id
	    If (Success_is in Dsid_debit_pend_del)
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Debit party ", Ent_debit_set.Dbt_typ,
			" pend delete.", / ;
                %End
	    ELSE
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Not Str Thru: Ambiguous debit party ",
                    Ent_debit_set.Dbt_typ / ;
                %End
	    END-IF
            Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            Set Failure_is in Debitside_look_status_ls to true
            Set Success_is in Dsid_debit_adr_erred to TRUE
        end-if.
	
	IF (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE )
	   OR ( (Dbt_ovr of Dbt_typ of Ent_debit_set = "*" )
	    	AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
* We actually did get a REL hit or an AUX db hit, so let's copy the address.
	THEN
%^      Do not increment database to initialize fields
	    If (Adr_name_length of Relget_adr_set_lengths = ZERO )
	       AND (Dbt_name1_length of Ent_debit_set_lengths NOT = ZERO )
	    THEN
 	        %Beg  Ent_debit_set.Dbt_name1 = NULL;  %End
	    END-IF
	    If (Adr1_length of Relget_adr_set_lengths = ZERO )
		AND (Dbt_name2_length of Ent_debit_set_lengths NOT = ZERO )
	    THEN
 	        %Beg  Ent_debit_set.Dbt_name2 = NULL;  %End
	    END-IF
	    If (Adr2_length of Relget_adr_set_lengths = ZERO )
		AND (Dbt_name3_length of Ent_debit_set_lengths NOT = ZERO )
	    THEN
 	        %Beg  Ent_debit_set.Dbt_name3 = NULL;  %End
	    END-IF
	    If (Adr3_length of Relget_adr_set_lengths = ZERO )
		AND (Dbt_name4_length of Ent_debit_set_lengths NOT = ZERO )
	    THEN
 	        %Beg  Ent_debit_set.Dbt_name4 = NULL;  %End
	    END-IF
 
%^ map the name and address from debit address party 
	    Perform C440_COPY_ADDRESS through C440_COPY_ADDRESS_END
	END-IF.
        perform b280_debit_party_exit_rtn thru b280_debit_party_exit_rtn_end.


B280_DEBIT_PARTY_CLEANUP.

	%^ Ensure that we fill in res_country if possible

	If dbt_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "DBT"
	       by reference dbt_idtype of Ent_debit_set
	       by reference dbt_id of Ent_debit_set
	       by reference dbt_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference dbt_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.dbt_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

B280_DEBIT_PARTY_END.

   EXIT.

B280_DEBIT_PARTY_EXIT_RTN.
	IF (Dsid_rel_id NOT = 0 )
	    %Beg  
	    Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;
            Ent_debit_set (
                .Dbt_rel_id      =      Ent_d_adr_set.Rel_id,
                .Dbt_adr_bnk_id  =      Ent_d_adr_set.Bnk_id,
                .Dbt_adr_set_ptr POINT: Ent_d_adr_set,
		.flgs3.dbt_adr_ptr_ok = "T") ;
	    %End
            If (Bank of Loc_info of Ent_ftr_set = Bnk_id of Ent_d_adr_set )
                If (Dbt_adr_bnk_id of Ent_debit_set NOT =
                                                     Bnk_id of Ent_d_adr_set )
                    %Beg
                    Ent_debit_set.Dbt_adr_bnk_id = Ent_d_adr_set.Bnk_id ;
                    %End
                END-IF
            ELSE
                If (Bank of Loc_info of Ent_ftr_set = SPACES )
                    %Beg
                    Ent_ftr_set.loc_info.bank = Ent_d_adr_set.Bnk_id ;
                    Ent_debit_set.Dbt_adr_bnk_id = Ent_d_adr_set.Bnk_id ;
                    %End
                ELSE
%^ Already initialized to a different bank.
                    Set success_is in Msg_bank_changed_ls to true
                    If (Failure_is in Nochange_bank_ls )
                        %Beg
                        Ent_ftr_set.loc_info.bank = Ent_d_adr_set.Bnk_id ;
                        Ent_debit_set.Dbt_adr_bnk_id = Ent_d_adr_set.Bnk_id ;
                        %End
                      else
                        %^ Don't change the bank after all
                        Set Failure_is in Msg_bank_changed_ls to true
                        %Beg
                        Ent_debit_set( .Dbt_typ.dbt_ovr    = "*",
                                       .Dbt_rel_id         = <0> ,
                                       .Dbt_account        = NULL,
                                       .Dbt_acc_class      = NULL,
                                       .Dbt_acc_prod_codes = NULL,
                                       .Flgs.dbt_hold_flg  = NULL,
                                       .Dbt_department     = NULL,
                                       .Dbt_recon_ref      = NULL,
                                       .Dbt_name1          = NULL,
                                       .Dbt_name2          = NULL,
                                       .Dbt_name3          = NULL,
                                       .Dbt_name4          = NULL,
                                       .Dbt_adr_set_ptr DELETE,
				       .dbt_res_country    = NULL,
			   	       .flgs3.dbt_adr_ptr_ok = Null ) ;
                        %End
* NOF debit party.  Set debit party bank to something reasonable.
                        Call "SET_NOF_DBT_BNK_ID" using
			    by reference Nochange_bank_ls
	                    by reference Dsid_loc_bank_change
	                  RETURNING Dsid_ret_stat
			If (Dsid_loc_bank_change NOT = 0 )
			    Set Success_is in Msg_bank_changed_ls to true
			END-IF
                        %^ Write an error memo indicating the discrepancy.
                        %Beg
                        Dsid_compose ^OUT(Dsid_err_memo)
                            "Debit party: ",
                            Ent_debit_set.dbt_typ.dbt_idtype,"/",
                            Ent_debit_set.dbt_typ.dbt_id,
                            " conflicts with message bank: "
                            Ent_ftr_set.loc_info.bank, / ;
                        %End
                        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
                    END-IF
                END-IF
            END-IF
	ELSE
	    If  Dbt_rel_id of Ent_debit_set not = 0 then
                %Beg
                Ent_debit_set(  .Dbt_rel_id        = <0> ,
                                .DBt_adr_set_ptr DELETE ) ;
                %End
	    END-IF
	    %^ If AUX, make to hookup
	    IF (Dbt_ovr of Dbt_typ of Ent_debit_set = "*" )
	    	AND (Relget_msgcode = Vmsg_dat_notonfile_wc )  THEN
	    %beg
		    Ent_debit_set (.Dbt_adr_set_ptr POINT: Relget_adr_set,
				   .flgs3.DBT_adr_ptr_ok = "T" );
	    %end
	    GO TO B280_debit_party_exit_rtn_end
	END-IF.

	IF (Dbt_rel_id of Ent_debit_set = 0 )
%^ NOF debit party cannot have a repetitive associated with it.
	    Move "F" to Rptv_lookup_flg_ws
	    Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
	    MOVE "N" to Dbt_comm_charge_ws
	    MOVE "N" to Dbt_cbl_charge_ws
* NOF debit party.  Set debit party bank to something reasonable.
            Call "SET_NOF_DBT_BNK_ID" using
		by reference Nochange_bank_ls
	        by reference Dsid_loc_bank_change
	      RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		Set Success_is in Msg_bank_changed_ls to true
	    END-IF
            If (Success_is in Msg_bank_changed_ls)
                AND (Success_is in Nochange_bank_ls )
            THEN
                %^ Write an error memo indicating the discrepancy.
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id,
                    " conflicts with message bank: "
                    Ent_ftr_set.loc_info.bank, / ;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
            If (Failure_is in Dsid_ret_stat  )
                %^ Write an error memo indicating the problem
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Invalid bank in debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id, /;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
	END-IF.
B280_debit_party_exit_rtn_end.
    exit.
B300_SECOND_DEBIT.
%^ Paragraph checks debit address once we have decided on one against second
%^ debit identifier from message.  We return match status in
%^ Second_debit_matched_ls but we will continue trying to find account
%^ regardless.   Returned status has already been initialized to failure.
                                    
	EVALUATE Dsid_checkidt_ws
	    WHEN "N"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
			Sname_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "A"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
			Aba_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF
	
	    WHEN "S"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
			Swift_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "B"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
			Branch_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "C"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
		      Chips_uid_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "U"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
		        User_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "I"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
		        Interbnk_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

	    WHEN "K"
	        IF (Dsid_checkid_ws(1:Dsid_checkid_ws_length) = 
		        Customer_id of Ent_d_adr_set(1:Dsid_checkid_ws_length) )
	        THEN
		    Move 1 to Second_dbt_matched_ls
	        END-IF

            WHEN OTHER
	        If Dsid_checkid_ws_length < 15 then
* Must be an account reference.  Check the account sequence for it.
		    %Beg
 		    SEARCH: Dsid_acc_seq (GEQ, .Rel_name_key
					   (.idbank = Ent_d_adr_set.Bnk_id,
					    .idtype = Dsid_checkidt_ws,
					    .idkey.idacc = Dsid_checkid_ws ) ) ;
 		    %End
		    If (Success_is in Dsid_acc_seq_status   )
		       AND (Dsid_checkidt_ws = Idtype of Rel_name_key
		    				       of Dsid_acc_seq)
                       AND (Dsid_checkid_ws(1:Dsid_checkid_ws_length) =
		    	Idacc of Idkey of Rel_name_key of Dsid_acc_seq 
			(1:Dsid_checkid_ws_length) )
		    THEN
		        Move 1 to Second_dbt_matched_ls
                        %Beg
                        Ent_debit_set.dbt_typ.dbt_idtype = Dsid_checkidt_ws;
                        Dsid_compose ^OUT(Ent_debit_set.dbt_typ.dbt_id)
				Dsid_checkid_ws, / ;
                        %End
		    END-IF
		    %Beg  Dsid_acc_seq(EQL) ;  %End
		END-IF

	END-EVALUATE.
	
B300_SECOND_DEBIT_END.
   EXIT.
B320_SETUP_2ND_ID.
	Set Failure_is in Dsid_itsa_clearhouse to TRUE.
	Move SPACE to Dsid_2aid_idt.
	Move SPACE to Dsid_2bid_idt.
	Move SPACES to Dsid_party_id.
	%Beg
	Dsid_2aid_id = NULL ;
	Dsid_2bid_id = NULL ;
	Dsid_2id_ws = NULL ;
	Dsid_2id_temp_ws = NULL;
	%End
	If (Second_dbt_id_ls_length > 70 )
	    OR (Second_dbt_id_ls_length NOT > 0 )
	THEN
	    GO TO B320_SETUP_2ND_ID_END
	END-IF.
	If (Second_dbt_id_ls(1:1) NOT = "/" )
	    Move "/" to Dsid_2id_temp_ws
	    Move Second_dbt_id_ls(1:Second_dbt_id_ls_length) to
		   Dsid_2id_temp_ws(2:Second_dbt_id_ls_length)
	    Add 1 to Second_dbt_id_ls_length giving Dsid_2id_temp_ws_length
	ELSE
	    Move Second_dbt_id_ls_length to Dsid_2id_temp_ws_length
	    Move Second_dbt_id_ls(1:Dsid_2id_temp_ws_length) to Dsid_2id_temp_ws
	END-IF
	%Beg
	Dsid_party_id = NULL ;
	Dsid_party_extype = NULL ;
	Dsid_parse ^IN(Dsid_2id_temp_ws)
		Dsid_2id_oneof( ^ONEOF(
			("/AC",|Dsid_party_id, ^SPACE, /),
			("/", |Dsid_party_extype(^STRING<2>(<CHAR$M_ALPHA>)),
						    |Dsid_party_id ^SPACE, /),
			(/) ));
        %End.
 	If (Failure_is in Dsid_parse_status  )
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Incorrect second debit ID ", Dsid_2id_temp_ws, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Move ZERO to Second_dbt_matched_ls
	    Set Failure_is in Debitside_look_status_ls to true
	    GO TO B320_SETUP_2ND_ID_END
        END-IF.

 	EVALUATE TRUE
	    When (Dsid_2id_oneof = 0 )
		Move "D" to Dsid_party_idtype
	    When (Dsid_2id_oneof = 2 )
		Move Space to Dsid_party_idtype
	    When Dsid_party_extype = "BC"
		Move "S" to Dsid_party_idtype
	    When Dsid_party_extype = "CH"
		Move "C" to Dsid_party_idtype
	    When Dsid_party_extype = "FW"
		Move "A" to Dsid_party_idtype

	    When Other
		%^ Must search for a clearinghouse record for our source.
		CALL "GET_PID_ADV_SUFFIX" Using
		    By reference Bank of Loc_info of Ent_ftr_set
		    By reference Src_code of Ent_ftr_set
		    By reference Dsid_clrhs_suffix
		    By reference Dsid_clrhs_suffix_length
		  Returning Dsid_itsa_clearhouse
		If (Success_is in Dsid_itsa_clearhouse)
		    Move "P" to Dsid_party_idtype
		ELSE
		    Move "E" to Dsid_party_idtype
		    %Beg
		    Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
			Dsid_party_id, / ;
		    %End
		    Move Dsid_temp1_vstr(1:Dsid_temp1_vstr_length) To
			Dsid_party_id (3:)
		    Move Dsid_party_extype To Dsid_party_id (1:2)
		    Add 2 To Dsid_party_id_length
		END-IF

	END-EVALUATE.
	Set Success_is in Dsid_gcheck_actodd to true
	Perform X940_TAKEOUT_GARBAGE through X940_TAKEOUT_GARBAGE_END.
	If (Success_is in Dsid_had_garbage   )
            %Beg
            Dsid_compose ^OUT(Dsid_err_memo)
			"Incorrect second debit ID ", Dsid_2id_temp_ws, / ;
            %End
            Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Move ZERO to Second_dbt_matched_ls
            Set Failure_is in Debitside_look_status_ls to true
	    GO TO B320_SETUP_2ND_ID_END
        END-IF.

	%Beg
	Dsid_2aid_id = NULL ;
	Dsid_2bid_id = NULL ;
	Dsid_party_extype2 = NULL;
	Dsid_parse ^IN(Dsid_party_id) Dsid_2aid_id, 
		Dsid_twoof_acchfw (^ONEOF (	
			("/AC",|Dsid_2bid_id, ^SPACE, /),
			("/",  |Dsid_party_extype2(^STRING<2>(<CHAR$M_ALPHA>)),
			  			    |Dsid_2bid_id ^SPACE, / ),
			(^SPACE, /) ));
	%End.
	Move Dsid_party_idtype to Dsid_2aid_idt.
	If (Failure_is in Dsid_parse_status  )
	    %Beg
	    Dsid_2aid_id = NULL ;
	    Dsid_2bid_id = NULL ;
	    Dsid_parse ^IN(Dsid_party_id) 
			Dsid_2aid_id, ^SPACE, /  ;
	    %End
	    GO TO B320_SETUP_2ND_ID_END
	END-IF.
	If (Dsid_2aid_idt = "P")
	    %Beg
	    Dsid_temp1_vstr = Dsid_2aid_id;
	    Dsid_clip_compose ^OUT(Dsid_2aid_id)
		Dsid_temp1_vstr, Dsid_clrhs_suffix, / ;
	    %End
	END-IF.
	Set Failure_is in Dsid_itsa_clearhouse to TRUE.
	If (Dsid_twoof_acchfw = 2 )
	    %Beg
	    Dsid_2bid_id = NULL ;
	    Dsid_2bid_idt = NULL ;
	    Dsid_party_extype2 = NULL ;
	    %End
	ELSE
	    Evaluate True
		When Dsid_twoof_acchfw = 0
		    Move "D" to Dsid_2bid_idt
		When Dsid_party_extype2 = "BC"
		    Move "S" to Dsid_2bid_idt
		When Dsid_party_extype2 = "CH"
		    Move "C" to Dsid_2bid_idt
		When Dsid_party_extype2 = "FW"
		    Move "A" to Dsid_2bid_idt
		When Other
		%^ Must search for a clearinghouse record for our source.
		CALL "GET_PID_ADV_SUFFIX" Using
		    By reference Bank of Loc_info of Ent_ftr_set
		    By reference Src_code of Ent_ftr_set
		    By reference Dsid_clrhs_suffix
		    By reference Dsid_clrhs_suffix_length
		  Returning Dsid_itsa_clearhouse
		If (Success_is in Dsid_itsa_clearhouse)
		    Move "P" to Dsid_party_idtype
		ELSE
		    Move "E" to Dsid_party_idtype
		    %Beg
		    Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
			Dsid_party_id, / ;
		    %End
		    Move Dsid_temp1_vstr(1:Dsid_temp1_vstr_length) To
			Dsid_party_id (3:)
		    Move Dsid_party_extype To Dsid_party_id (1:2)
		    Add 2 To Dsid_party_id_length
		END-IF

	    END-EVALUATE
	END-IF.

	If (Dsid_2aid_id_length > 19 )
	   OR (Dsid_2bid_id_length > 19 )
	THEN
%^ Illegal identifier.
	    Move SPACE to Dsid_2aid_idt
	    Move SPACE to Dsid_2bid_idt
	    Move SPACES to Dsid_2aid_id
	    Move SPACES to Dsid_2bid_id
	    %Beg
	    Dsid_2aid_id = NULL ;
	    Dsid_2bid_id = NULL ;
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Illegal format in second debit ID ", 
			Dsid_2id_temp_ws, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Move ZERO to Second_dbt_matched_ls
	    Set Failure_is in Debitside_look_status_ls to true
	    GO TO B320_SETUP_2ND_ID_END
        END-IF.
	If (Dsid_2bid_idt = "P")
	    %Beg
	    Dsid_temp1_vstr = Dsid_2bid_id;
	    Dsid_clip_compose ^OUT(Dsid_2bid_id)
		Dsid_temp1_vstr, Dsid_clrhs_suffix, / ;
	    %End
	END-IF.

B320_SETUP_2ND_ID_END.


   EXIT.
C400_FIND_ADR_CUR.
*
*  We have an ambiguous debit party address.
*  We also have a non-blank currency which we can use to resolve the
*  ambiguity.  We will scan through all of the addresses and check
*  the currencies on their associated accounts.  If we find only one
*  address with a matching currency, we have successfully disambiguated.
*  If we are in "nocheck_ambig" mode, we will return the first address
*  with an account in the correct currency.
        
	%ace_conn_root_q Rel_index ;
	Set Success_is in Dsid_next_status to true.
	Move ZERO to Dsid_qualified_rel.
	PERFORM WITH TEST BEFORE UNTIL (Failure_is in Dsid_next_status   )
	    Move Rel_id of Relget_adr_set to Dsid_rel_id
	    %Beg
	    BREAK: Dsid_adr_set ;
	    BREAK: Dsid_acc_seq ;
	    Rel_index ^SEARCH (Key = Dsid_rel_id);
	    %End
	    If (Success_is in Rel_index_status   )
		AND (OBJECT_IS in Rel_index_cursor )
		AND (ADDRESS_IS in Rel_type of Rel_index)
	    THEN
		%Beg
		Rel_index CONN: Dsid_adr_set(NOMOD) ;
		Dsid_adr_set.account_seq CONN: 
					Dsid_acc_seq ^FIRST (NOMOD) ;
                %End

		If (Failure_is in Xbank_account_ok_ls )
* Position ourselves into the correct bank -- it's the top of the key
	    	    Perform UNTIL 
			(Idbank of Rel_name_key of Dsid_acc_seq =
                                        Bnk_id of Dsid_adr_set )
			  OR (Failure_is in Dsid_acc_seq_status   )
			%Beg  NEXT: Dsid_acc_seq ;  %End
	    	    END-PERFORM
		END-IF
		PERFORM with TEST BEFORE UNTIL
			(Failure_is in Dsid_acc_seq_status)
		        OR ( (Failure_is in Xbank_account_ok_ls )
                             AND ( Bnk_id of Dsid_adr_set NOT =
			      	      Idbank of Rel_name_key of Dsid_acc_seq) )
*  Truck through the account sequence looking for a matching currency.
		    PERFORM C480_CHECK_CURRENCY through C480_CHECK_CURRENCY_END
		    If (Success_is in Dsid_curr_okay )
*  Found account with matching currency.
			If (Dsid_qualified_rel = 0 )
			    Move Rel_id of Dsid_adr_set to Dsid_qualified_rel
			    If (Dsid_ambig_ws = "T" )
* First hit is acceptable, so we are done.
				GO TO C400_FIND_ADR_CUR_EXIT
			    END-IF
			ELSE
* We already have a hit.  Shucky darns, we are still ambiguous.
			    Move ZERO to Dsid_qualified_rel
			    GO TO C400_FIND_ADR_CUR_EXIT
			END-IF
		    END-IF
                    %Beg  NEXT: Dsid_acc_seq ;  %End
		END-PERFORM
            END-IF
	    Call "NEXT_ACCT_LOOKUP" using
	        by reference Dbt_idtype of Dbt_typ of Ent_debit_set
	        by reference Dbt_id of Dbt_typ of Ent_debit_set
	        by reference Dsid_ovr
	      RETURNING Dsid_next_status
        END-PERFORM.
C400_FIND_ADR_CUR_EXIT.
	%Beg
	BREAK: Dsid_adr_set ;
	BREAK: Dsid_acc_seq ;
        %End.
C400_FIND_ADR_CUR_END.

   EXIT.
C410_DBT_SWF_PARENT.
%^    If the SWIFT key length is 11, then try a lookup by the 1st 8 characters.
%^    If found, and there is a DDA, then assume the branch is debiting its
%^ parent account.
%^    Subsequent checking will determine if the branch has debit authority over 
%^ the parent.
%^ Assume that the sender will be output to the sending bank fields
%^ If name/address of the original debit party was found on auxiliary database,
%^     copy it
	%Beg
	Dsid_id_ws = NULL ;
	Compose ^Out(Dsid_tmp_id_ws) Ent_debit_set.dbt_typ.dbt_id,/;
	Dsid_Parse ^IN(Dsid_tmp_id_ws),
	 	^ONEOF( (^STRING, ":", Dsid_id_ws, ^ONEOF(/, "/")),
	  		(Dsid_id_ws, ^ONEOF(/, "/")) );
	%End.
	If (Dsid_id_ws_length NOT = 11)
	    GO TO C410_DBT_SWF_PARENT_END
	END-IF.


%^ Try lookup using 8 character key
	Set dbt in Relget_title_flag to true.
	Move spaces to Dsid_id_ws(9:3).
	Move 8 to Dsid_id_ws_length.
	Set DBT in Relget_title_flag to TRUE.
	Move "F" to Dsid_lkup_pend_del
	Call "ACCT_LOOKUP" using
	    by reference Dbt_idtype of Dbt_typ of Ent_debit_set
	    by reference Dsid_id_ws
	    by reference Dsid_2nd_ovr
	    by content "F"
	    by reference Dsid_multibank_ws
	    by reference Debit_currency_ls
	    by reference Dsid_lkup_pend_del
	 RETURNING Dsid_ret2_stat.

        If Dsid_lkup_pend_del = "T"
%^ Parent address is pending delete!
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
		"Debit party ", Ent_debit_set.Dbt_typ.Dbt_idtype, "/",
			Ent_debit_set.Dbt_typ.Dbt_id, " parent "
			Dsid_id_ws, " is marked for delete", / ;
 	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Add 1 to Dsid_ambig_parties
	    Set Failure_is in Debitside_look_status_ls to true
        End-if. 

%^ Lookup failed - no action, remove side effects so caller copies name & addr
	If (Dsid_2nd_ovr NOT = SPACE )
* Just replace our divot by re-doing the initial lookup
	    %Beg  BREAK: Dsid_acctyp_seq;  %End
	    Set DBT in Relget_title_flag to TRUE
	    Move "F" to Dsid_lkup_pend_del
	    Call "ACCT_LOOKUP" using
	        by reference Dbt_idtype of Dbt_typ of Ent_debit_set
	        by reference Dbt_id of Dbt_typ of Ent_debit_set
	        by reference Dsid_ovr
	        by reference Dsid_ambig_ws 
	        by reference Dsid_multibank_ws
	        by reference Debit_currency_ls
	        by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret_stat
	    GO TO C410_DBT_SWF_PARENT_END
	END-IF.


%^ The 8 character ID is in the REL file.  Make it the debit party.
	%Beg
	Ent_debit_set.Dbt_typ.Dbt_ovr = Dsid_2nd_ovr ;
	Ent_debit_set.Dbt_typ.Dbt_id = Dsid_id_ws ;
	Dsid_ovr = " " ;

%^ And now lets add an info memo saying what happened
	Dsid_compose ^OUT(Dsid_info_memo)
		"SWF branch ", Ent_debit_set.SBK.Sbk_idtype, "/", 
		Ent_debit_set.SBK.Sbk_id, " inserted -> ", 
		Ent_debit_set.Dbt_typ.Dbt_idtype, "/", 
		Ent_debit_set.Dbt_typ.Dbt_id," pushed down. " , / ;
	%End.
	Perform X920_INFO_MEMO through X920_INFO_MEMO_END.
C410_DBT_SWF_PARENT_END.


   EXIT.
C440_COPY_ADDRESS.
%^
	If (Adr_name_length of Relget_adr_set_lengths = ZERO )
	   AND (Dbt_name1_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name1 = NULL;  %End
	END-IF.
	If (Adr1_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name2_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name2 = NULL;  %End
	END-IF.
	If (Adr2_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name3_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name3 = NULL;  %End
	END-IF.
	If (Adr3_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name4_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name4 = NULL;  %End
	END-IF.
%^
	%Beg
	Dsid_Compose Relget_adr_set (
		.Adr_name (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name1), ^_, /)),
		.Adr1 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name2), ^_, /)),
		.Adr2 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name3), ^_, /)),
		.Adr3 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name4), ^_, /))) ;
%^ Build a DBT_ADR_TYPE field from country code and 
%^   ADR_TYPE field of Relget_adr_set
	Dsid_Compose ^OUT(Ent_debit_set.Dbt_adr_type),
		Relget_adr_set.Country_code(^STRING<2>),
		Relget_adr_set.Adr_type, /;
	%End.

%^ copy in the zip code too
	If (Zip of Relget_adr_set NOT = SPACES )
	    Call "ZIPSUB" Using
		by reference Dbt_name4 of Ent_debit_set
		by reference Dbt_name4_length of Ent_debit_set_lengths
		by reference Dbt_name3 of Ent_debit_set
		by reference Dbt_name3_length of Ent_debit_set_lengths
		by reference Zip of Relget_adr_set
		by reference Line_flg_ws

	    EVALUATE Line_flg_ws
		WHEN "4"
		    %Beg  Ent_debit_set.Dbt_name4 CHANGE;  %End

		WHEN "3"
		    %Beg  Ent_debit_set.Dbt_name3 CHANGE;  %End

	    END-EVALUATE
	END-IF.
C440_COPY_ADDRESS_END.


   EXIT.
C480_CHECK_CURRENCY.
* Checks currency of account in Dsid_acc_seq against message.  If they 
* match,  Success_is is in Dsid_curr_okay.  Else Failure_is is in it.

	If (Debit_currency_ls = SPACES )
* Message currency is unknown /don't care.
	    Set Success_is in Dsid_curr_okay to TRUE
	    GO TO C480_CHECK_CURRENCY_END
	END-IF.

	If (Idbank of Rel_name_key of Dsid_acc_seq = Bnk_id of Menu_bnk_union )
	    Move Base_currency_id of Menu_bnk_union to Dsid_acc_curr_ws
	ELSE
	    If (Idbank of Rel_name_key of Dsid_acc_seq = Dsid_acc_bank_ws )
%^ Note caching here to try to cut down on lookups.
		Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
	    ELSE
%^  Rats.  We have to look up the bank.
		%Beg
		BREAK: Dsid_bnk_union ;
		SEARCH: Bnk_index 
			(Key = Dsid_acc_seq .Rel_name_key.Idbank ) ;
		%End
		If Failure_is in Bnk_index_status  
		    Move SPACES to Dsid_acc_curr_ws
		ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Bnk_Id of Dsid_bnk_union to Dsid_acc_bank_ws
		    Move Base_currency_id of Dsid_bnk_union to 
			Dsid_bank_curr_ws
		    Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
		END-IF
	    END-IF
	END-IF.
	If (Disp_cur of Dsid_acc_seq NOT = SPACES )
	    Move Disp_cur of Dsid_acc_seq to Dsid_acc_curr_ws
	END-IF.	
	If (Dsid_acc_curr_ws = Debit_currency_ls )
	    Set Success_is in Dsid_curr_okay to TRUE 
	ELSE
	    Set Failure_is in Dsid_curr_okay to TRUE
	END-IF.
C480_CHECK_CURRENCY_END.

	EXIT.
C500_LOWER_DEBIT.

* Finds subsidiary debitside party which may be identified by multiple IDs. 
* Full ID is in Dsid_party_idtype and Dsid_party_id.  Status of lookup is 
* returned in Dsid_ret_stat and Dsid_ovr.  All IDs must agree for the lookup to
* succeed and have its address used (Dsid_ovr = SPACE, Dsid_ret_stat SUCCESS).
* If any ID cannot be done, name and address should not be used but lookup
* has not failed, either.  Dsid_ret_stat is success, but Dsid_ovr is *.
* If no ID could be done, name and address do not exist but lookup has not
* failed.  Dsid_ret_stat is FAILURE and Dsid_ovr is *.
* If any two IDs conflict, Dsid_ret_stat is FAILURE and Dsid_ovr is ?.
*
%^ We still need to try the lookup
	Move "*" to Dsid_ovr.
	Set Success_is in Dsid_ret_stat to true.
	Set Failure_is in Dsid_itsa_clearhouse to TRUE.
	Set Failure_is in Dsid_lc_nowadr to TRUE.
	%Beg  
	Dsid_lc_aba = NULL ;
	Dsid_lc_account = NULL ;
	Dsid_lc_partic = NULL ;
	Dsid_lc_swift = NULL ;
	Dsid_lc_uid = NULL ;
        Dsid_lc_user = NULL ;
	Dsid_lc_extid = NULL ;
	Dsid_party_extype = NULL ;
	Dsid_lc_flg_aba = " " ;
	Dsid_lc_flg_account = " " ;
	Dsid_lc_flg_partic = " " ;
	Dsid_lc_flg_swift = " " ;
	Dsid_lc_flg_uid = " " ;
        Dsid_lc_flg_user = " " ;
	Dsid_lc_flg_extid = " " ;
	Dsid_acchfwid = NULL ;
	Dsid_id_ws = NULL ;
	Dsid_lc_adridt = NULL ;
	Dsid_lc_adrid = NULL ;
%^ Could be a compound id with an /AC, /BC, /CH, or /FW or similar form
	Dsid_Parse ^In(Dsid_party_id), Dsid_id_ws,
		    Dsid_oneof_acchfw (^ONEOF (	
			("///",|Dsid_acchfwid, ^SPACE, /),
			("/AC",|Dsid_acchfwid, ^SPACE, /),
			("/", |Dsid_party_extype (^STRING<2>(<CHAR$M_ALPHA>)),
			   			   |Dsid_acchfwid, ^SPACE, /),
			(/) ));
        %End.

	If Failure_is in Dsid_parse_status  
	    %Beg  Dsid_acchfwid = NULL;  Dsid_party_extype = NULL; %End
	    Move 4 To Dsid_oneof_acchfw
	END-IF.

	Move 2 to Dsid_twoof_acchfw.		%^ Default is 2-char subtype
	Evaluate TRUE
	    When Dsid_party_idtype = "D"
		Move 0 to Dsid_twoof_acchfw
		Move "AC" To Dsid_party_extype2

	    When Dsid_party_idtype = SPACE
		Move 1 to Dsid_twoof_acchfw
		Move "AC" To Dsid_party_extype2

	    When Dsid_party_idtype = "S"
		Move "BC" To Dsid_party_extype2

	    When Dsid_party_idtype = "C"
		Move "CH" To Dsid_party_extype2

	    When (Dsid_party_idtype = "P" )
%^ NOTE: if the ID is in P-form, it must already have any necessary suffix.
%^ Since this is a lower debitside party, we're just trying to get its
%^ extended Id form
		If Dbt_adr_bnk_id of Ent_debit_set = SPACES
		    Move Bank of Loc_info of Ent_ftr_set to Dsid_acc_bank_ws
		ELSE
		    Move Dbt_adr_bnk_id of Ent_debit_set to Dsid_acc_bank_ws
		END-IF
		CALL "GET_XIDTYPE_FROM_SUFFIX" Using
		    By reference Dsid_acc_bank_ws
		    By reference Dsid_id_ws
		    By Reference Dsid_id_ws_length
		    By Reference Dsid_party_extype2
		  Returning Dsid_itsa_clearhouse

	    When Dsid_party_idtype = "A"
		Move "FW" To Dsid_party_extype2

	    When Dsid_party_idtype = "E"
%^ #45981 If the string following the code word of the extended ID sub-type of
%^        "ID" were alpha characters, an ambiguous error message was written for
%^        the beneficiary.  By moving a "3" to Dsid_oneof_acchfw and 
%^        Dsid_twoof_acchfw, the ambiguous conditions are not encountered.
		If (dsid_oneof_acchfw = 2 And
		    dsid_acchfwid(3:1) = "/") Then 	%^ We have a 4 character id type
							%^ deal with it
			Set Failure_is in Dsid_ret_stat	to True	%^ 111825
			Go To c500_lower_debit_end
			%^ We cannot validate, sent it thru
		End-if
                If Dsid_id_ws (1:2) = "ID"
		Then
                    Move 3 to Dsid_oneof_acchfw
                    Move 3 to Dsid_twoof_acchfw
		End-if		
		If (dsid_id_ws = Spaces and %^ all id is in acchfwid Mapper shouldn't allow this
		    dsid_oneof_acchfw = 2) Then %^ Kick out  spr 80869
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END			
		end-if
 		Move Dsid_id_ws(1:2) To Dsid_party_extype2
		%Beg
		Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
		    Dsid_id_ws, / ;
		%End
		Subtract 2 from Dsid_temp1_vstr_length
		Move Dsid_temp1_vstr(3:Dsid_temp1_vstr_length) To Dsid_id_ws
		If Dsid_id_ws_length > 1 Then 
		    Subtract 2 From Dsid_id_ws_length 
		End-if

            When Dsid_party_idtype = "U"  	%^ User Name
	        %^ Is this a multi-party id?
                %Beg
                Dsid_parse ^IN(Dsid_id_ws)
                    ^STRING, "/", ^STRING, /;
                %end
                If Failure_is in Dsid_parse_status   then
		    %^ Single-Party
                    Move 4 to Dsid_twoof_acchfw
                  else
                    %^ Multi-Party
                    Move 3 to Dsid_twoof_acchfw
                end-if
                Move Spaces To Dsid_party_extype2

            When OTHER
		Move 3 to Dsid_twoof_acchfw
		Move Spaces To Dsid_party_extype2

	END-EVALUATE.

	Perform UNTIL ( Dsid_id_ws_length = 0 )

	    Evaluate Dsid_twoof_acchfw
		When 1
* It's a foreign account
		    If (Dsid_lc_account_length NOT = 0 )
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END
		    END-IF
		    %Beg  
		    Dsid_lc_account = Dsid_id_ws ;
		    Dsid_lc_flg_account = "F" ;
		    %End

		When 2
* It's a network id with a 2-character subtype equivalent
		  Evaluate Dsid_party_extype2
		   When "BC"					%^SWIFT TID
		    If (Dsid_lc_swift_length NOT = 0 )
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END
		    END-IF
		    Move SPACES to Dsid_lc_swift
		    %Beg  
		    Dsid_lc_swift = Dsid_id_ws ;
		    Dsid_lc_flg_swift = "F" ;
		    %End

		  When "CH"				%^ It's a CHIPS UID
		    If (Dsid_lc_uid_length NOT = 0 )
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END
		    END-IF
		    %Beg  
		    Dsid_lc_uid = Dsid_id_ws ;
		    Dsid_lc_flg_uid = "F" ;
		    %End

		  When "FW"				%^ It's a FED ABA
		    If (Dsid_lc_aba_length NOT = 0 )
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END
		    END-IF
		    %Beg  
		    Dsid_lc_aba = Dsid_id_ws ;
		    Dsid_lc_flg_aba = "F" ;
		    %End

		  When Other				
%^ Other 2-char external ID
		    If (Dsid_lc_extid_length NOT = 0 )
			Move "?" to Dsid_ovr
			Set Failure_is in Dsid_ret_stat to true
			GO TO C500_LOWER_DEBIT_END
		    END-IF
		    %Beg  
		    Dsid_compose ^Out(Dsid_lc_extid), 
			Dsid_party_extype2, Dsid_id_ws, /;
		    Dsid_lc_flg_extid = "F" ;
		    %End

	         END-EVALUATE

             When 4
* It's a User Name
                 If (Dsid_lc_user_length NOT = 0 )
                     Move "?" to Dsid_ovr
                     Set Failure_is in Dsid_ret_stat to true
                     GO TO C500_LOWER_DEBIT_END
                 END-IF
                 %Beg
                 Dsid_lc_user = Dsid_id_ws ;
                 Dsid_lc_flg_user = "F" ;
                 %end

	    END-EVALUATE

	    %Beg
	    Dsid_temp1_vstr = Dsid_acchfwid ;
	    Dsid_acchfwid = NULL ;
	    Dsid_id_ws = NULL ;
	    Dsid_twoof_acchfw = Dsid_oneof_acchfw ;
	    Dsid_party_extype2 = Dsid_party_extype;
	    Dsid_oneof_acchfw = <0> ;
	    %End
	    If (Dsid_temp1_vstr_length NOT = 0 )
		%Beg
		Dsid_Parse ^In(Dsid_temp1_vstr), Dsid_id_ws,
		    Dsid_oneof_acchfw (^ONEOF (	
			("///",|Dsid_acchfwid, ^SPACE, /),
			("/AC",|Dsid_acchfwid, ^SPACE, /),
			("/", |Dsid_party_extype (^STRING<2>(<CHAR$M_ALPHA>)),
			   			    |Dsid_acchfwid, ^SPACE, /),
			(/) ));
                %End
	    END-IF

	END-PERFORM.

* We now have parsed the entire lower debitside id set.  So now we want to
* look it up, somehow.
	If Dsid_lc_flg_uid = "F"	
* We have a CHIPS UID, so let's use this as our primary key
	    %Beg
	    Dsid_temp_idtype = "c" ;
	    Dsid_id_ws = Dsid_lc_uid ;
	    %End
	    Move Spaces to Dsid_lkup_pend_del	
 	    Call "ACCT_LOOKUP" USING
	    	by reference Dsid_temp_idtype
		by reference Dsid_id_ws
		by reference Dsid_ovr
		by reference Dsid_ambig_ws 
		by reference Dsid_multibank_ws
	      	by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret2_stat
*
* If not found on AUX then try on REL
*
	    If NOT ( (Dsid_ovr = "*" )
		 AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	    THEN
	       %Beg Dsid_temp_idtype = "C" ; %End
	       Call "ACCT_LOOKUP" USING
	    	   by reference Dsid_temp_idtype
		   by reference Dsid_id_ws
		   by reference Dsid_ovr
		   by reference Dsid_ambig_ws 
		   by reference Dsid_multibank_ws
	      	   by reference Debit_currency_ls
		   by reference Dsid_lkup_pend_del
	       RETURNING Dsid_ret2_stat

            End-if

	    If (Dsid_ovr = SPACE)
		Move "Y" to Dsid_lc_flg_uid
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
		Perform D600_MATCH_IDS_TO_REL through D600_MATCH_IDS_TO_REL_END 
		GO TO C500_LOWER_DEBIT_CLEANUP
	    END-IF
	    If ( (Dsid_ovr = "*" )
		AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	    THEN
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
		If (Dsid_lc_flg_swift NOT = SPACE )
		    If (Dsid_lc_swift(1:11) = 
			Swift_id of Relget_adr_set(1:11) )
		    THEN
			Move "Y" to Dsid_lc_flg_swift
		        If (Dsid_lc_flg_aba NOT = SPACE )
			    Perform D620_SWIFT_TO_ABA through
                                 D620_SWIFT_TO_ABA_END
			    If (Success_is in Dsid_ret2_stat   )
				If (Dsid_lc_aba(1:9) = Idkey of Disp_id
					of Dsid_aux_index(1:9))
				THEN
				    Move "Y" to Dsid_lc_flg_aba
				ELSE
				    Move "N" to Dsid_lc_flg_aba
				END-IF
			    END-IF   
			END-IF
		    ELSE
                        If (Swift_id_length of Relget_adr_set_lengths NOT = 0 )
                           AND (Swift_id of Relget_adr_set NOT = SPACES )
                        THEN
                            Move "N" to Dsid_lc_flg_swift
                        END-IF
		    END-IF
		END-IF
	    END-IF
	    GO TO C500_LOWER_DEBIT_CLEANUP
	END-IF.

	If (Dsid_lc_flg_ABA = "F" )
* NOTE: We know that we cannot have a chips UID, though we can have the
*	others.
	    %Beg
	    Dsid_temp_idtype = "A" ;
	    Dsid_id_ws = Dsid_lc_aba ;
	    %End
	    Move Spaces to Dsid_lkup_pend_del	
	    Call "ACCT_LOOKUP" USING
	    	by reference Dsid_temp_idtype
		by reference Dsid_id_ws
		by reference Dsid_ovr
		by reference Dsid_ambig_ws 
		by reference Dsid_multibank_ws
	      	by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret2_stat
	    If (Dsid_ovr = SPACE)
		Move "Y" to Dsid_lc_flg_aba
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
		Perform D600_MATCH_IDS_TO_REL through D600_MATCH_IDS_TO_REL_END 
		GO TO C500_LOWER_DEBIT_CLEANUP
	    END-IF
	    If ( (Dsid_ovr = "*" )
		AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	    THEN
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
		If (Dsid_lc_flg_swift NOT = SPACE )
		    Perform D620_SWIFT_TO_ABA through D620_SWIFT_TO_ABA_END
		    If (Success_is in Dsid_ret2_stat   )
			If (Dsid_lc_aba(1:9) = Idkey of Disp_id
					of Dsid_aux_index(1:9))
			THEN
			    Move "Y" to Dsid_lc_flg_swift
			ELSE
			    Move "N" to Dsid_lc_flg_swift
			END-IF
		    END-IF
		END-IF
	    ELSE
		Set Failure_is in Dsid_lc_nowadr to TRUE
	    END-IF
	    GO TO C500_LOWER_DEBIT_CLEANUP
	END-IF.

	If (Dsid_lc_flg_partic = "F" )
%^ NOTE: We know that we cannot have a chips UID or an ABA, though we can have 
%^       a chips participant.
	    %Beg
	    Dsid_temp_idtype = "P" ;
	    Dsid_id_ws = Dsid_lc_partic ;
	    %End
	    Move Spaces to Dsid_lkup_pend_del	
	    Call "ACCT_LOOKUP" USING
	    	by reference Dsid_temp_idtype
		by reference Dsid_id_ws
		by reference Dsid_ovr
		by reference Dsid_ambig_ws 
		by reference Dsid_multibank_ws
	      	by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret2_stat
	    If (Dsid_ovr = SPACE)
		Move "Y" to Dsid_lc_flg_partic
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
		Perform D600_MATCH_IDS_TO_REL through D600_MATCH_IDS_TO_REL_END 
* Since we don't have chips participants in the AUX database, we are done.
	    ELSE
		Set Failure_is in Dsid_lc_nowadr to TRUE
	    END-IF
	    GO TO C500_LOWER_DEBIT_CLEANUP
	END-IF.

	If (Dsid_lc_flg_swift = "F" )
%^ NOTE: We know that we cannot have a chips UID or participant or an ABA, 
%^	so this had better just be a SWIFT
	    %Beg
%^ This searches for the SWIFT TID in the AUX SWIFT database
	    Dsid_temp_idtype = "$" ;
	    Dsid_id_ws = Dsid_lc_swift ;
	    %End
	    Move Spaces to Dsid_lkup_pend_del	
	    Call "ACCT_LOOKUP" USING
	    	by reference Dsid_temp_idtype
		by reference Dsid_id_ws
		by reference Dsid_ovr
		by reference Dsid_ambig_ws 
		by reference Dsid_multibank_ws
	      	by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret2_stat
* Cannot get an on-rel hit here; only on-aux.
	    If ( (Dsid_ovr = "*" )
		AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	    THEN
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
* Let us take shameless advantage of the fact that there cannot
*   be any more idtypes to cross-check.
		Move "Y" to Dsid_lc_flg_swift
	    ELSE
		%Beg
            %^ This searches for the SWIFT TID in the CHIPS UID database
		Dsid_temp_idtype = "s" ;
		Dsid_id_ws = Dsid_lc_swift ;
		%End
		Move Spaces to Dsid_lkup_pend_del	
	        Call "ACCT_LOOKUP" USING
	    	    by reference Dsid_temp_idtype
		    by reference Dsid_id_ws
		    by reference Dsid_ovr
		    by reference Dsid_ambig_ws 
		    by reference Dsid_multibank_ws
	      	    by reference Debit_currency_ls
		    by reference Dsid_lkup_pend_del
	          RETURNING Dsid_ret2_stat
* Cannot get an on-rel hit here; only on-aux.
	        If ( (Dsid_ovr = "*" )
		    AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	        THEN
		    %Beg
		    Dsid_lc_adridt = Dsid_temp_idtype;
		    Dsid_lc_adrid = Dsid_id_ws;
		    %End
		    Set Success_is in Dsid_lc_nowadr to TRUE
* Let us take shameless advantage of the fact that there cannot
*   be any more idtypes to cross-check.
		    Move "Y" to Dsid_lc_flg_swift
	        ELSE
		    %Beg
%^ This searches for the SWIFT TID in the REL database
		    Dsid_temp_idtype = "S" ;
		    Dsid_id_ws = Dsid_lc_swift ;
		    %End
		    Move Spaces to Dsid_lkup_pend_del	
	            Call "ACCT_LOOKUP" USING
	    	        by reference Dsid_temp_idtype
		        by reference Dsid_id_ws
		        by reference Dsid_ovr
		        by reference Dsid_ambig_ws 
		        by reference Dsid_multibank_ws
	      	        by reference Debit_currency_ls
			by reference Dsid_lkup_pend_del
	              RETURNING Dsid_ret2_stat
                    If Dsid_ovr = SPACE
			%Beg
			Dsid_lc_adridt = Dsid_temp_idtype;
			Dsid_lc_adrid = Dsid_id_ws;
			%End
			Set Success_is in Dsid_lc_nowadr to TRUE
		        Move "Y" to Dsid_lc_flg_swift
	            ELSE
			If ( (Dsid_ovr = "*" )
			    AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	        	THEN
		            Move "Y" to Dsid_lc_flg_swift
			    %Beg
			    Dsid_lc_adridt = Dsid_temp_idtype;
			    Dsid_lc_adrid = Dsid_id_ws;
			    %End
			    Set Success_is in Dsid_lc_nowadr to TRUE
			ELSE
			    Set Failure_is in Dsid_lc_nowadr to TRUE
			END-IF
		    END-IF
		END-IF
	    END-IF
	END-IF.

        If (Dsid_lc_flg_user = "F" )		%^ User Name
            %Beg
            Dsid_temp_idtype = "U" ;
            Dsid_id_ws = Dsid_lc_user ;
            %End
	    %^ Look up address
	    Move Spaces to Dsid_lkup_pend_del	
            Call "ACCT_LOOKUP" USING            
                by reference Dsid_temp_idtype
                by reference Dsid_id_ws
                by reference Dsid_ovr
                by reference Dsid_ambig_ws
                by reference Dsid_multibank_ws
                by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
              RETURNING Dsid_ret2_stat

            Move "N" to Dsid_lc_flg_user

            If (Dsid_ovr not = SPACE)
		Set Failure_is in Dsid_lc_nowadr to TRUE
                GO TO C500_LOWER_DEBIT_CLEANUP
            END-IF
	    %Beg
	    Dsid_lc_adridt = Dsid_temp_idtype;
	    Dsid_lc_adrid = Dsid_id_ws;
	    %End
	    Set Success_is in Dsid_lc_nowadr to TRUE

            %^ Change to preferred idtype - leave universal types alone
            IF (Dsid_temp_idtype = "S" OR "C" OR "P" OR "A") THEN
                Move "Y" to Dsid_lc_flg_user
              else
                %^ Change to best available universal key using values in
                %^ Relget_adr_set
                CALL "FIND_ADR_ID" USING
                    BY REFERENCE
                        Dsid_temp_idtype
                        Dsid_find_adr_idtype
                        Dsid_find_adr_id
                        Dsid_find_adr_id_length
                    RETURNING Dsid_ret2_stat
                IF Success_is in Dsid_ret2_stat  
                  then
                    %Beg
                    Dsid_party_idtype = dsid_find_adr_idtype;
                    Dsid_party_id = Dsid_find_adr_id;
                    %End
                    Move "Y" to Dsid_lc_flg_user
                end-if
            END-IF
        END-IF.

%^ Extended ids
        If (Dsid_lc_flg_extid = "F" )
            %Beg
            Dsid_temp_idtype = "e" ;    %^ look in Aux_ext_id_index
            Dsid_id_ws = Dsid_lc_extid ;
            %End
	    Move Spaces to Dsid_lkup_pend_del	
            Call "ACCT_LOOKUP" USING
                by reference Dsid_temp_idtype
                by reference Dsid_id_ws
                by reference Dsid_ovr
                by reference Dsid_ambig_ws
                by reference Dsid_multibank_ws
                by reference Debit_currency_ls
		by reference Dsid_lkup_pend_del
              RETURNING Dsid_ret2_stat
            If ( (Dsid_ovr = "*" )
                AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
            THEN
                Move "Y" to Dsid_lc_flg_extid   %^found on Aux_ext_id_index
		%Beg
		Dsid_lc_adridt = Dsid_temp_idtype;
		Dsid_lc_adrid = Dsid_id_ws;
		%End
		Set Success_is in Dsid_lc_nowadr to TRUE
            ELSE
                %Beg                      %^ search REL for extended_id
                Dsid_temp_idtype = "E" ;
                Dsid_id_ws = Dsid_lc_extid ;
                %End
		Move SPACES to Dsid_lkup_pend_del
		Move Dsid_ambig_ws to Dsid_ambig_save_ws
%^		No ambig check for BCC Lookup of BIC
		If (Lkup_bic_by_bcc_ws = "D" or "B")
	           Move "T" to Dsid_ambig_ws
		End-if

                Call "ACCT_LOOKUP" USING
                    by reference Dsid_temp_idtype
                    by reference Dsid_id_ws
                    by reference Dsid_ovr
                    by reference Dsid_ambig_ws
                    by reference Dsid_multibank_ws
                    by reference Debit_currency_ls
		    by reference Dsid_lkup_pend_del
                  RETURNING Dsid_ret2_stat

		Move Dsid_ambig_save_ws to Dsid_ambig_ws 

                If Dsid_ovr = SPACE
                    Move "Y" to Dsid_lc_flg_extid
		    %Beg
		    Dsid_lc_adridt = Dsid_temp_idtype;
		    Dsid_lc_adrid = Dsid_id_ws;
		    %End
		    Set Success_is in Dsid_lc_nowadr to TRUE
                ELSE
                    If ( (Dsid_ovr = "*" )
                        AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
                    THEN
                        Move "Y" to Dsid_lc_flg_extid
			%Beg
			Dsid_lc_adridt = Dsid_temp_idtype;
			Dsid_lc_adrid = Dsid_id_ws;
			%End
			Set Success_is in Dsid_lc_nowadr to TRUE
		    ELSE
			Set Failure_is in Dsid_lc_nowadr to TRUE
                    END-IF
                END-IF
            END-IF
        END-IF.

C500_LOWER_DEBIT_CLEANUP.
* See how we fared.
	Set Failure_is in Dsid_ret_stat to true
	If (Dsid_lc_flg_aba = "N" )
           OR (Dsid_lc_flg_account = "N" )
           OR (Dsid_lc_flg_partic = "N" )
           OR (Dsid_lc_flg_swift = "N" )
	   OR (Dsid_lc_flg_uid = "N" )
	   OR (Dsid_lc_flg_extid = "N" )
           OR (Dsid_lc_flg_user = "N" )
	THEN
* Hard failure -- we have a conflict.
            Move "?" to Dsid_ovr
	    GO TO C500_LOWER_DEBIT_END
	END-IF.
	If (Dsid_lc_flg_aba NOT = "Y" )
           AND (Dsid_lc_flg_account NOT = "Y" )
           AND (Dsid_lc_flg_partic NOT = "Y" )
           AND (Dsid_lc_flg_swift NOT = "Y" )
	   AND (Dsid_lc_flg_uid NOT = "Y" )
	   AND (Dsid_lc_flg_extid NOT = "Y" )
           AND (Dsid_lc_flg_user NOT = "Y" )
	THEN
* Soft failure -- we have no hits.
	    Move "*" to Dsid_ovr
	    GO TO C500_LOWER_DEBIT_END
	END-IF.

	Set Success_is in Dsid_ret_stat to true
	If (Dsid_lc_flg_aba NOT = "F" )
           AND (Dsid_lc_flg_account NOT = "F" )
           AND (Dsid_lc_flg_partic NOT = "F" )
           AND (Dsid_lc_flg_swift NOT = "F" )
	   AND (Dsid_lc_flg_uid NOT = "F" )
	   AND (Dsid_lc_flg_extid NOT = "F" )
           AND (Dsid_lc_flg_user NOT = "F" )
	THEN
* Hard success -- We could look up everything we found
	    Move SPACE to Dsid_ovr
	ELSE
* Soft success -- We have some leftovers.
	    Move "*" to Dsid_ovr
	    If (Dsid_lc_adrid_length NOT = 0 )
	      AND (Dsid_lc_adrid NOT = SPACES )
	      AND (Failure_is in Dsid_lc_nowadr)
	    THEN
%^ Get the name and address info back in the relget_adr_set.
		Move Spaces to Dsid_lkup_pend_del
                Call "ACCT_LOOKUP" USING
                    by reference Dsid_lc_adridt
                    by reference Dsid_lc_adrid
                    by reference Dsid_ovr
                    by reference Dsid_ambig_ws
                    by reference Dsid_multibank_ws
                    by reference Debit_currency_ls
		    by reference Dsid_lkup_pend_del
                  RETURNING Dsid_temp_stat
	    END-IF
   	END-IF.
C500_LOWER_DEBIT_END.

   EXIT.

D100_BCC_LKUP_BIC.

%^ If configured, replace "E" BCC Code with the BIC

%^ Assume successful processing without BCC replacement
	Set Success_is in Bcc_lkup_status to true.
	Set Failure_is in Bcc_now_bic to true.

%^ Exit if not valid for processing 
	IF (Lkup_bic_by_bcc_ws = "D" OR "B") AND
	   (Relget_return_idtype = "E")
	    Continue
	ELSE
	    Go to D100_BCC_LKUP_BIC_END
	END-IF.

%^ If SWIFT ID not available, set error;
%^ Otherwise, replace BCC with BIC
%^ FTRSCR_EDITS will perform final validation
	IF Swift_id of Relget_adr_set = spaces 
	    Set Failure_is in Dsid_ret_stat to true
	    Set Failure_is in Dsid_found_it to true
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"SWIFT ID required: ", Bcc_lkup_party, " party.", / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Set Failure_is in Bcc_lkup_status to true
	ELSE
	    %Beg
	    Dsid_party_idtype = "S";
	    Dsid_compose ^OUT(Dsid_party_id),
			Relget_adr_set.Swift_id, /;
	    Relget_return_idtype = "S";
	    Dsid_compose ^OUT(Relget_return_key),
			Relget_adr_set.Swift_id, /;
	    %End
	    Set Success_is in Bcc_now_bic to true
	END-IF.

D100_BCC_LKUP_BIC_END.
   EXIT.

D600_MATCH_IDS_TO_REL.

* Paragraph takes relget_adr_set and matches any "F"(ound) flagged IDs to
*  it if it can.

	If (Dsid_lc_flg_aba = "F" )
	    If (Dsid_lc_aba(1:Dsid_lc_aba_length) = Aba_id of Relget_adr_set )
		Move "Y" to Dsid_lc_flg_aba
	    ELSE
		Move "N" to Dsid_lc_flg_aba
	    END-IF
	END-IF.

	If (Dsid_lc_flg_partic = "F" )
	    %Beg
	    BREAK: Dsid_acc_seq ;
	    Relget_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
 	    SEARCH: Dsid_acc_seq (EQL, .Rel_name_key
					(.idbank = Relget_adr_set.Bnk_id,
					 .idtype = "P",
					 .idkey = Dsid_lc_partic ) ) ;
            Dsid_ret2_stat = Dsid_acc_seq status ;
	    BREAK: Dsid_acc_seq ;
 	    %End
	    If (Success_is in Dsid_ret2_stat   )
		Move "Y" to Dsid_lc_flg_partic
	    ELSE	      
		Move "N" to Dsid_lc_flg_partic
	    END-IF
	END-IF.
			
	If (Dsid_lc_flg_swift = "F" )
	    If (Dsid_lc_swift(1:11) = Swift_id of Relget_adr_set(1:11) )
		Move "Y" to Dsid_lc_flg_swift
	    ELSE
		Move "N" to Dsid_lc_flg_swift
	    END-IF
	END-IF.

	If (Dsid_lc_flg_uid = "F" )
	    If (Dsid_lc_uid(1:Dsid_lc_uid_length) = 
					      Chips_uid_id of Relget_adr_set )
		Move "Y" to Dsid_lc_flg_uid
	    ELSE
		Move "N" to Dsid_lc_flg_uid
	    END-IF
	END-IF.

	If (Dsid_lc_flg_extid = "E" )
		%beg
		BREAK: relget_supp_id_seq;
		Relget_adr_set.Supplement_id_seq CONN: Relget_supp_id_seq ^FIRST (nomod);
		%end
		Move "N" to Dsid_lc_flg_extid
		Perform until seq_end_is in relget_supp_id_seq_cursor
			If Dsid_lc_extid(1:Dsid_lc_extid_length) =
				Idacc of Idkey of Rel_name_key of relget_supp_id_seq
			  then	Move "Y" to Dsid_lc_flg_extid
				%beg
				END: relget_supp_id_seq;
				%end
			  else	%beg
				NEXT: relget_supp_id_seq;
				%end
			End-if
		End-perform
	END-IF.

D600_MATCH_IDS_TO_REL_END.

   EXIT.
D620_SWIFT_TO_ABA.
* Paragraph to look up SWIFT ID in the SWIFT to ABA INDEX.
* If we got a hit, Dsid_ret2_stat is SUCCESS and the ABA is in Disp_id of
*    Dsid_aux_index.
	%ACE_IS Dsid_aux_set connected returning Ace_status_wf ;.
	If Failure_is in Ace_status_wf
	    %Beg  dat_root_set.Aux_db_set CONN: Dsid_aux_set(NOMOD);  %End
	END-IF.
	Move SPACES to Dsid_scr_adr_id.
	%Beg
	BREAK: Dsid_aux_index ;
	Dsid_scr_adr_id.Idtype = "S" ;
	Dsid_compose ^OUT(Dsid_scr_adr_id.idkey.idacc), 
				    Dsid_lc_swift, / ;
	Dsid_aux_set(.Swf_to_aba_index CONN: Dsid_aux_index ^SEARCH
			(READ_ONLY, 
			 EQL,
			 .Rel_name_key = Dsid_scr_adr_id ) );
	Dsid_ret2_stat = Dsid_aux_index Status;
	BREAK: Dsid_aux_index ;
	%End.
D620_SWIFT_TO_ABA_END.

   EXIT.

* Utility paragraphs.

   EXIT.
X900_ERROR_MEMO.
*  This paragraph writes an error memo using the text string in Dsid_err_memo.
	If Dsid_err_memo_length = 0 
	    GO TO X900_ERROR_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_err_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_err_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_err_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)
	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
	ADD 1 TO Error_memo_count_ls.
X900_ERROR_MEMO_END.


   EXIT.
X920_INFO_MEMO.
*  This paragraph writes an informational trace memo using the text string
*      in Dsid_info_memo.
	If Dsid_info_memo_length = 0 
	    GO TO X920_INFO_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_info_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_info_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_info_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)

	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname = "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
	ADD 1 TO Dsid_adr_info_count.
X920_INFO_MEMO_END.

   EXIT.
X940_TAKEOUT_GARBAGE.
* This paragraph does sanity checks on a full id string passed for a debitside
* party by a mapper.  This id string contains not only the initial id but also,
* appended to it, the overflow or second ids. 
* There are two components to the check, though they are done together in a
* single pass through the Dsid_party_idtype and the Dsid_party_id strings.
* First we validate the ids by idtype as we encounter them (correct number of 
* characters, correct numeric/alpha) and we also search for a 
* "start of garbage" identifier "/??".
* If we find a "start of garbage" we compress it out of the string and return
* Dsid_had_garbage as SUCCESS.  
* If any id fails its validation test we stop scanning and return 
* Dsid_had_garbage as SUCCESS
* Otherwise we return Dsid_had_garbage as FAILURE.
* Dsid_gcheck_processed is constructed in parallel as our modified string.

	Set Failure_is in Dsid_had_garbage to true.
        Set Failure_is in Dsid_bad_dda to true.
	Set Failure_is in Dsid_had_ac to true.

%^ Determine if the Id has a bbb: prefix.  If so, strip it off and check it
%^ against the bank index to determine if it's valid.
        %Beg
        Dsid_parse ^IN(Dsid_party_id)
            Dsid_party_bank_id, ":", Dsid_gcheck_remain2, /;
        %End
        If Success_is in Dsid_parse_status   then
            %Beg SEARCH: Bnk_index (Key = Dsid_party_bank_id); %End
            If Failure_is in Bnk_index_status  
                Set Success_is in Dsid_had_garbage to true
            end-if
          else
            %Beg 
	    Dsid_party_bank_id = NULL; 
	    Dsid_gcheck_remain2 = Dsid_Party_id;
	    %End
        end-if.
        If ( Dsid_party_bank_id_length = 0 ) 
	   OR (Dsid_party_bank_id = SPACES )
	THEN
	    %Beg  Dsid_gcheck_bank_id = Ent_ftr_set.Loc_info.Bank;  %End
	ELSE
	    %Beg  Dsid_gcheck_bank_id = Dsid_party_bank_id;  %End
	END-IF.

	%Beg
	Dsid_party_extype2 = NULL ;
	Dsid_party_extype = NULL ;
	Dsid_party_intro = NULL ;
	Dsid_party_intro2 = NULL ;
	%End

	Evaluate TRUE
	    When Dsid_party_idtype = "D"
%^ Indicates an on-us account ID
		Set GCHT_DDA in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "G"
%^ Indicates an on-us account ID
		Set GCHT_GL in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "F"
%^ Indicates an on-us account ID
		Set GCHT_NOSTRO in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "V"
%^ Indicates an on-us account ID
		Set GCHT_SAV in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = " "
%^ Indicates another bank's account ID
		Set GCHT_AC in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "S"
		Set GCHT_SWF in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "C"
		Set GCHT_CHUSER in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "A"
		Set GCHT_FED in Dsid_gcheck_states to TRUE
	    When Dsid_party_idtype = "P"
		%^ Must search for a clearinghouse record for our source.
		CALL "GET_PID_ADV_DATA" Using
		    By reference Bank of Loc_info of Ent_ftr_set
		    By reference Src_code of Ent_ftr_set
		    By Reference Dsid_clrhs_adv
		    By reference Dsid_clrhs_swf_acc
		    By reference Dsid_clrhs_currency
		    By reference Dsid_clrhs_formatter
		    By reference Dsid_clrhs_min_length
		    By reference Dsid_clrhs_max_length
		    By Reference Dsid_clrhs_mod_check
		    By Reference Dsid_clrhs_mod_check_length
		    By Reference Dsid_clrhs_ch_name
		    By Reference Dsid_clrhs_ch_name_length
		    By reference Dsid_clrhs_suffix
		    By reference Dsid_clrhs_suffix_length
		    By reference Dsid_clrhs_clr_sys
		    By reference Dsid_clrhs_clr_sys_length
		    By reference Dsid_clrhs_destype
		    By reference Dsid_clrhs_destype_length
		    By reference Dsid_clrhs_dfincpy
		    By reference Dsid_clrhs_dfincpy_length
		    By reference Dsid_clrhs_lqm_fnc
		    By reference Dsid_clrhs_lqm_fnc_length
		    By reference Dsid_clrhs_clr_type
		    By reference Dsid_clrhs_clr_type_length
		    By reference Dsid_clrhs_clr_bic
		    By reference Dsid_clrhs_clr_bic_length
		    By reference Dsid_clrhs_clr_gl
		    By reference Dsid_clrhs_clr_gl_length
		    By reference Dsid_clrhs_clr_nstro
		    By reference Dsid_clrhs_clr_nstro_length
		  Returning Dsid_itsa_clearhouse
		If (Failure_is in Dsid_itsa_clearhouse)
		    Set GCHT_NONE in Dsid_gcheck_states to TRUE
		ELSE 
		    Move Dsid_clrhs_swf_acc To Dsid_party_extype
		    If (Dsid_party_extype = "CP")
			Set GCHT_CHIPS in Dsid_gcheck_states to TRUE
		    ELSE
			Set GCHT_CLEAR in Dsid_gcheck_states to TRUE
		    END-IF
		END-IF
		
	    When Dsid_party_idtype = "E"
%^ Parse subtype later
		Set GCHT_EXTEND in Dsid_gcheck_states to TRUE
%^ Parse subtype
		Move Dsid_gcheck_remain2(1:2) to Dsid_party_extype2
		Move Dsid_gcheck_remain2(3:) to Dsid_temp1_vstr
		If (Dsid_gcheck_remain2_length > 2 )
		    Subtract 2 from Dsid_gcheck_remain2_length giving
			Dsid_temp1_vstr_length
		ELSE
		    Move Zero to Dsid_temp1_vstr_length
		END-IF
		%Beg  Dsid_gcheck_remain2 = Dsid_temp1_vstr;  %End


            When OTHER
%^ No interesting IDTYPE
		Set GCHT_NOMATCH in Dsid_gcheck_states to TRUE
	END-EVALUATE.

	If (Dsid_gcheck_remain2(1:1) = "/" )
%^ Flush any duplicate mode indicator with a null actual id.
	    %Beg
	    Dsid_temp1_vstr = Dsid_gcheck_remain2 ;
%^ NOTE: ONEOF's below must match states for defined oneof Dsid_gcheck_states
	    Dsid_parse ^IN(Dsid_temp1_vstr)
		Dsid_gcheck_twoof( ^ONEOF(
		(/),                                     %^GCHT_NONE
            	("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_DDA
       		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_SAV
       		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_GL
            	("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_NOSTR
            	("/AC",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_AC
            	("/BC",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_SWF
            	("/CH",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CHUSER
            	("/FW",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_FED
            	("/CP",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CHIPS
            	("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CLEAR
            	("/??",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_GARBAGE
            	(/) ));
	    %End
            If (Dsid_gcheck_twoof NOT = Dsid_gcheck_states )
%^ Do not eat next mode indicator in id
		%Beg  Dsid_gcheck_remain2 = Dsid_temp1_vstr;  %End
	    END-IF
	END-IF.

	%Beg  
	Dsid_gcheck_processed = NULL ;
	Dsid_gcheck_twoof = Dsid_gcheck_states ;
	%End.

* If the idtype is "E", initialize for the loop below.  Prevous "E" idtype logic
* in this paragraph ensure the 2-character subtype is not in dsid_temp1_vstr.  \* The logic below also avoids messing with DSID_PARTY_ID in case it will be
* returned intact
	If Dsid_party_idtype = "E" Then
	    If (Dsid_party_bank_id_length NOT = 0 )
		AND (Dsid_party_bank_id NOT = SPACES )
	    THEN
	    	%Beg  
	    	Dsid_compose ^OUT(Dsid_gcheck_processed)
			Dsid_party_bank_id, ":",  Dsid_party_extype2, / ;
	        %End
	    ELSE
		%Beg  Dsid_gcheck_processed = Dsid_party_extype2 ;  %End
	    END-IF
	    %Beg
	    Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
		Dsid_gcheck_remain2, /;
	    Dsid_gcheck_remain2 = NULL;
	    Dsid_gcheck_remain2 = Dsid_temp1_vstr;
	    %End
	ELSE
	    If (Dsid_party_bank_id_length NOT = 0 )
		AND (Dsid_party_bank_id NOT = SPACES )
	    THEN
	    	%Beg  
	    	Dsid_compose ^OUT(Dsid_gcheck_processed)
			Dsid_party_bank_id, ":", / ;
	        %End
	    END-IF
	END-IF.

	If (GCHT_NOMATCH in Dsid_gcheck_states )
	   OR ( (GCHT_EXTEND in Dsid_gcheck_states)
		AND (Dsid_party_extype2 = "ID" ) )
	THEN
%^ Don't know how to check it, so just add it onto the output string and exit
	    %Beg
	    Dsid_compose ^OUT(Dsid_temp1_vstr)
		Dsid_gcheck_processed, Dsid_gcheck_remain2, / ;
	    Dsid_gcheck_processed = Dsid_temp1_vstr ;
	    %End
	    GO TO X940_TAKEOUT_GARBAGE_END
	END-IF.	    

	Perform UNTIL (Dsid_gcheck_remain2_length = 0 )
		      OR  (Success_is in Dsid_had_garbage   )
		      OR  (Dsid_gcheck_twoof = 0 )

	    Move Dsid_gcheck_twoof to Dsid_gcheck_oneof
	    %Beg  
	    Dsid_gcheck_states = Dsid_gcheck_oneof; 
	    Dsid_party_extype = Dsid_party_extype2;
	    Dsid_party_intro = Dsid_party_intro2 ;
	    Dsid_gcheck_remain = Dsid_gcheck_remain2 ;
	    Dsid_party_intro2 = NULL ;
	    Dsid_gcheck_remain2 = NULL ;
	    Dsid_party_extype = Dsid_party_extype ;
	    Dsid_gcheck_id = NULL ;
	    Dsid_party_extype2 = NULL ;
	    Dsid_gcheck_adr_id = NULL ;
	    %End

	    If (GCHT_DDA in Dsid_gcheck_states )
		OR (GCHT_SAV in Dsid_gcheck_states )
		OR (GCHT_GL in Dsid_gcheck_states)
		OR (GCHT_NOSTRO in Dsid_gcheck_states)
	    THEN
		%Beg
		Dsid_gcheck_adrof = <0> ;
		Dsid_gcheck_remain2 = NULL ;
		Dsid_temp3_vstr = NULL ;
	        Dsid_parse ^IN(Dsid_gcheck_remain)
		    Dsid_gcheck_id, Dsid_gcheck_twoof( ^ONEOF(
			(^SPACE, /),
			("/", / ),
			("/", ^SPACE, / ),
                        ("/", |Dsid_temp3_vstr, "/", |Dsid_gcheck_remain2, 
				^SPACE, / ),
                        ("/", |Dsid_temp3_vstr, ^SPACE, / ) ) ) ;
		%End
		If (Success_is in Dsid_parse_status   )
		    Evaluate Dsid_gcheck_adrof
			When 0
			    %Beg  
			    Dsid_gcheck_remain = Dsid_gcheck_id ;
			    %End

			When 1
			When 2
			    %Beg  
			    Dsid_gcheck_adr_id = "/" ;  
			    Dsid_gcheck_remain = Dsid_gcheck_id ;
			    %End

			When 3
			    If (Dsid_temp3_vstr_length > 0 )
				AND (Dsid_temp3_vstr_length < 6 )
		    	    THEN
%^ This case drops the address id.  The implicit "ELSE" case retains it.
				%Beg
				Dsid_compose ^OUT(Dsid_gcheck_adr_id)
				    "/", Dsid_temp3_vstr, / ;
				Dsid_compose ^OUT(Dsid_gcheck_remain)
				    Dsid_gcheck_id, "/", 
							Dsid_gcheck_remain2, / ;
				%End
			    END-IF

			When 4
			    If (Dsid_temp3_vstr_length > 0 )
				AND (Dsid_temp3_vstr_length < 6 )
		    	    THEN
				%Beg
				Dsid_compose ^OUT(Dsid_gcheck_adr_id)
				    "/", Dsid_temp3_vstr, / ;
				Dsid_gcheck_remain = Dsid_gcheck_id ;
				%End
			    END-IF
                   END-EVALUATE
		END-IF
	    END-IF

            If (Dsid_gcheck_remain_length NOT = 0 )
		%Beg
		Dsid_gcheck_remain2 = NULL ;

%^ NOTE: ONEOF's below must match states for defined oneof Dsid_gcheck_states
	        Dsid_parse ^IN(Dsid_gcheck_remain)
		    Dsid_gcheck_id, Dsid_gcheck_twoof( ^ONEOF(
			(/),                                     %^GCHT_NONE
            		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_DDA
            		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_SAV
            		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_GL
            		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_NOSTR
            		("/AC",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_AC
            		("/BC",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_SWF
            		("/CH",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CHUSER
            		("/FW",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_FED
            		("/CP",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CHIPS
            		("///",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_CLEAR
            		("/??",|Dsid_gcheck_remain2, ^SPACE, /), %^GCHT_GARBAGE
			("/",                                    %^GCHT_EXTEND
			  |Dsid_party_extype2 (^String<2>(<CHAR$M_ALPHA>)),
					      |Dsid_gcheck_remain2, ^SPACE, /),
            		(/) ));
	        %End
	    END-IF
	    If (Failure_is in Dsid_parse_status  )
%^ Note: this checks the earlier parse if Dsid_gcheck_remain was reset to NULL
		Set Success_is in Dsid_had_garbage to true
%^  Didn't parse it properly in the first place, so just stick it back.
		If (Dsid_gcheck_processed_length = 0 )
		    %Beg  Dsid_gcheck_processed = Dsid_gcheck_remain ;  %End
		ELSE
		    %Beg
		    Dsid_temp1_vstr = Dsid_gcheck_processed ;
		    Dsid_compose ^OUT(Dsid_gcheck_processed)
			Dsid_temp1_vstr, Dsid_gcheck_remain, / ;
		    %End
		END-IF
	    ELSE
		If (Dsid_gcheck_twoof = 0 )
		    %Beg  Dsid_party_intro2 = NULL;  %End
		ELSE
%^ Save the slash and 2 following characters
	            Add 1 to Dsid_gcheck_id_length giving Dsid_temp_long 
		    Move Dsid_gcheck_remain(Dsid_temp_long:3) 
							  to Dsid_party_intro2
		    Move 3 to Dsid_party_intro2_length
		END-IF
		If (Success_is in Dsid_gcheck_actodd )
		    If (GCHT_AC in Dsid_gcheck_states )
			Set GCHT_DDA in Dsid_gcheck_states to TRUE
		    END-IF
		END-IF
		EVALUATE TRUE 
		    When GCHT_DDA in Dsid_gcheck_states
%^ It's an on-us DDA account
			Move "D" To Dsid_acc_idtype_ws
			Perform X950_CHECK_ACCOUNT through
							X950_CHECK_ACCOUNT_END
			If Failure_is in Dsid_had_garbage  
                            Set Success_is in Dsid_bad_dda to true
			END-IF

		    When GCHT_SAV in Dsid_gcheck_states
			Move "V" To Dsid_acc_idtype_ws
			Perform X950_CHECK_ACCOUNT through
							X950_CHECK_ACCOUNT_END

		    When GCHT_GL in Dsid_gcheck_states
			Move "G" To Dsid_acc_idtype_ws
			Perform X950_CHECK_ACCOUNT through
							X950_CHECK_ACCOUNT_END

		    When GCHT_NOSTRO in Dsid_gcheck_states
			Move "F" To Dsid_acc_idtype_ws
			Perform X950_CHECK_ACCOUNT through
							X950_CHECK_ACCOUNT_END

		    When GCHT_AC in Dsid_gcheck_states
%^ It's somebody else's account number.
                        Move SPACES to Dsid_temp1_vstr   %^ NOP to make WHEN ok
		    When GCHT_SWF in Dsid_gcheck_states
%^ It's a SWIFT 2-character id-subtype
			CALL "VALIDATE_SWIFT" USING
			    By reference Dsid_gcheck_id
                            By reference Dsid_gcheck_id_length
			  RETURNING Dsid_ret2_stat
			If Failure_is in Dsid_ret2_stat  
			    Set Success_is in Dsid_had_garbage to true
			END-IF

		    When GCHT_CHUSER in Dsid_gcheck_states
%^ It's a CHIPS UID
			If (Dsid_gcheck_id_length = 6 )
                            CALL "VALIDATE_UID" USING
                                By reference Dsid_gcheck_id
                              RETURNING Dsid_ret2_stat
                            If (Failure_is in Dsid_ret2_stat  )
                                Set Success_is in Dsid_had_garbage to true
                            END-IF
			ELSE
                            Set Success_is in Dsid_had_garbage to true
			END-IF

		    When GCHT_FED in Dsid_gcheck_states
%^ It's a FEDWIRE ABA
			If (Dsid_gcheck_id_length = 9 )
			    CALL "VALIDATE_ABA" USING
			        By reference Dsid_gcheck_id
			      RETURNING Dsid_ret2_stat
			    If (Failure_is in Dsid_ret2_stat  )
			        Set Success_is in Dsid_had_garbage to true
			    END-IF
			ELSE
			    Set Success_is in Dsid_had_garbage to true
		        END-IF


		    When GCHT_CHIPS in Dsid_gcheck_states
%^ It's a CHIPS participant number in a non-U.S. environment
			If (Dsid_gcheck_id_length NOT = 4)
			   OR (Dsid_gcheck_id(1:Dsid_gcheck_id_length) 
								Is Not Numeric)
			THEN 
			    Set Success_is in Dsid_had_garbage to true
			END-IF


		    When GCHT_CLEAR in Dsid_gcheck_states
%^ Its a non-CHIPS "P" idtype
			CALL "CUST_P_CHECK_DIGIT" USING
			    BY REFERENCE Bnk_id of Menu_bnk_union
			    BY REFERENCE Dsid_gcheck_id
			    BY REFERENCE Dsid_gcheck_id_length
			    BY REFERENCE Dsid_check_stat
			  RETURNING Dsid_ret2_stat
			If Failure_is in Dsid_check_stat   Then
			    Set Success_is in Dsid_had_garbage to true
		        END-IF

                    When GCHT_GARBAGE in Dsid_gcheck_states
%^ Throw away the introducer and flag as garbage
			%Beg  Dsid_party_intro = NULL;  %End
 			Set Success_is in Dsid_had_garbage to true

 		    When GCHT_EXTEND in Dsid_gcheck_states
			Evaluate Dsid_party_extype
%^ Subtype determines action.
			    When "ID"
%^ Cannot check ID.  ID can contain slashes, so we don't even know
%^ when it ends.  But we don't have garbage.
				If (Dsid_gcheck_processed_length = 0 )
				    %Beg
				    Dsid_compose ^OUT(Dsid_gcheck_processed)
					Dsid_party_intro, Dsid_gcheck_id,
	                      		Dsid_party_intro2, Dsid_gcheck_remain2,
					/ ;
				    %End
				ELSE
				    %Beg
				    Dsid_temp1_vstr = Dsid_gcheck_processed ;
				    Dsid_gcheck_processed = NULL ;
				    Dsid_compose ^OUT(Dsid_gcheck_processed)
					Dsid_temp1_vstr, Dsid_party_intro, 
					Dsid_gcheck_id, Dsid_party_intro2, 
					Dsid_gcheck_remain2, / ;
				    %End
				%Beg
				Dsid_gcheck_id = NULL ;
				Dsid_gcheck_remain = NULL ;
				Dsid_gcheck_remain2 = NULL ;
				Dsid_party_intro = NULL;
				Dsid_party_intro2 = NULL;
				%End
				END-IF

			    When "CP"
			    When "AU" 
			    When "CC" 
			    When "SW" 
			    When "BL" 
			    When "SC" 
			    When "IT"
%^ Other SWIFT 2-character id's for which we don't currently have fancy edits
				Continue

			End-evaluate

		END-EVALUATE
%^ Now reconstruct the part of the input ID that we just processed.
		If (Success_is in Dsid_had_garbage  )
		    If (Dsid_gcheck_processed_length = 0 )
		        %Beg
		        Dsid_compose ^OUT(Dsid_gcheck_processed)
			    Dsid_party_intro, Dsid_gcheck_id, 
			    Dsid_party_intro2, Dsid_gcheck_remain2, / ;
		        %End
		    ELSE
		        %Beg
		        Dsid_temp1_vstr = Dsid_gcheck_processed ;
		        Dsid_gcheck_processed = NULL ;
		        Dsid_compose ^OUT(Dsid_gcheck_processed)
			    Dsid_temp1_vstr, Dsid_party_intro, 
			    Dsid_gcheck_id, Dsid_party_intro2,
			    Dsid_gcheck_remain2, / ;
		        %End
		    END-IF
		ELSE
		    If (Dsid_gcheck_processed_length = 0 )
		        %Beg
		        Dsid_compose ^OUT(Dsid_gcheck_processed)
			    Dsid_party_intro, Dsid_gcheck_id, / ;
		        %End
		    ELSE
			If Not (Success_is in dsid_on_us_bnk_code) Then
		 		%^ Is true, we have already changed to id, leave it
		        	%Beg
			       	   Dsid_temp1_vstr = Dsid_gcheck_processed ;
			       	   Dsid_gcheck_processed = NULL ;
			           Dsid_compose ^OUT(Dsid_gcheck_processed)
					    Dsid_temp1_vstr, Dsid_party_intro, 
							    Dsid_gcheck_id, / ;
		        	%End
		        end-if
		    END-IF
		END-IF
	    END-IF
	END-PERFORM.

	If (Success_is in Dsid_had_garbage   )
%^ Make sure that we haven't left a "/??" embedded in the id string
	    %Beg
	    Dsid_temp1_vstr = NULL ;
	    Dsid_parse ^IN(Dsid_gcheck_processed)
		^OPTION(Dsid_temp1_vstr), "/??", Dsid_temp2_vstr, / ;
	    %End
	    Perform UNTIL (Failure_is in Dsid_parse_status   )
		%Beg
		Dsid_compose ^OUT(Dsid_gcheck_processed)
			Dsid_temp1_vstr, Dsid_temp2_vstr, / ;
	        Dsid_parse ^IN(Dsid_gcheck_processed)
		        Dsid_temp1_vstr, "/??", Dsid_temp2_vstr, / ;
		%End
	    END-PERFORM
	END-IF.
	%Beg  Dsid_party_id = Dsid_gcheck_processed;  %End.

X940_TAKEOUT_GARBAGE_END.  


   EXIT.
X950_CHECK_ACCOUNT.
%^ Common code to call check digit for an account.
%^ Dsid_acc_idtype_ws contains the account IDTYPE.
%^ Looked-up (and possibly modified) account id is returned in Dsid_gcheck_id.
%^ Dsid_had_garbage is returned SUCCESS if checkdigit edit fails.
	%Beg Dsid_gcheck_dda_id = Dsid_gcheck_id; %End.
%^ Do the check-digit edit, possibly changing an old style DDA to new
	If Dsid_party_bank_id_length = 0 then
	    CALL "CHKDGT_EDIT" USING
		by reference Dsid_acc_idtype_ws
		by reference Dsid_gcheck_id
		by reference Dsid_gcheck_id_length
		by reference Bank of Loc_info of Ent_ftr_set
		by reference Dsid_multibank_ws
		by reference Dsid_temp1_vstr
	      RETURNING Dsid_ret2_stat
	ELSE
	    CALL "CHKDGT_EDIT" USING
		by reference Dsid_acc_idtype_ws
                by reference Dsid_gcheck_id
                by reference Dsid_gcheck_id_length
		by reference Dsid_party_bank_id
		by reference Dsid_multibank_ws
		by reference Dsid_temp1_vstr
	      RETURNING Dsid_ret2_stat
	END-IF.
%^ Now concatenate any address id back on.
	If (Dsid_gcheck_adr_id_length NOT = 0 )
	    %Beg
	    Dsid_gcheck_dda_id = Dsid_gcheck_id ;
	    Dsid_compose ^OUT(Dsid_gcheck_id)
		Dsid_gcheck_dda_id, Dsid_gcheck_adr_id, / ;
	    %End
	END-IF.
	If Failure_is in Dsid_ret2_stat
	   If ( DBT in Relget_title_flag And
	        dsid_acc_idtype_ws = "D" ) Then
		%^ Still a possibility of an ON US IBAN
		%^
		Perform X970_CHECK_FOR_IBAN thru
			X970_CHECK_FOR_IBAN_END
	   else
		   Set Success_is in Dsid_had_garbage to true
	   end-if
	END-IF.
X950_CHECK_ACCOUNT_END.


	EXIT.
X960_SHUFFLE_IDS.

%^ This paragraph shuffles the multiple id string in Dsid_id_ws and moves
%^ a space into Dsid_idtype_ws.  We have an /AC foreign account id as a
%^ second (or third or ...) id.  It will be shuffled into the first
%^ position and whatever we have instead will move down.

	%Beg
	Dsid_parse ^IN(Dsid_party_id)
		Dsid_shuffle_alt, "/(", Dsid_shuffle_parens, / ;
	%End.
	If (Failure_is in Dsid_parse_status)
	    %Beg  
	    Dsid_shuffle_parens = NULL ;  
	    Dsid_shuffle_alt = Dsid_party_id ;
	    %End
	END-IF.

	Move SPACE to Dsid_shuffle_idt
	%Beg
	Dsid_shuffle_part1 = NULL ;
	Dsid_shuffle_part2 = NULL ;
	Dsid_shuffle_acct = NULL ;
	Dsid_shuffle_oneof = <0> ;
	Dsid_parse ^IN(Dsid_shuffle_alt)
		Dsid_shuffle_part1, "/AC", Dsid_shuffle_acct, 
		Dsid_shuffle_oneof ( ^ONEOF (	
            		  ("///",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("///",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/BC",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/CH",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/FW",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/CP",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/??",|Dsid_shuffle_part2, ^SPACE, /),
            		  (/)));

        %End.
	If (Failure_is in Dsid_parse_status)
%^ Try for an account instead
	    Move "C" to Dsid_shuffle_idt
	    %Beg
	    Dsid_shuffle_part1 = NULL ;
	    Dsid_shuffle_part2 = NULL ;
	    Dsid_shuffle_acct = NULL ;
	    Dsid_shuffle_oneof = <0> ;
	    Dsid_parse ^IN(Dsid_shuffle_alt)
		Dsid_shuffle_part1, "/CH", Dsid_shuffle_acct, 
		Dsid_shuffle_oneof ( ^ONEOF (	
            		  ("///",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/AC",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/BC",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("///",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/FW",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/CP",|Dsid_shuffle_part2, ^SPACE, /),
            		  ("/??",|Dsid_shuffle_part2, ^SPACE, /),
            		  (/)));

            %End
	    If (Failure_is in Dsid_parse_status)
%^  This is, after all, just cleanup.
	        GO TO X960_SHUFFLE_IDS_END
	    END-IF
	END-IF.
	EVALUATE TRUE
	    WHEN Dsid_party_idtype = SPACE OR "D"
%^ Leave it alone
	        GO TO X960_SHUFFLE_IDS_END

	    WHEN Dsid_party_idtype = "S"
		Move "BC" to Dsid_shuffle_type

	    WHEN Dsid_party_idtype = "C"
		Move "CH" to Dsid_shuffle_type

	    WHEN Dsid_party_idtype = "A"
		Move "FW" to Dsid_shuffle_type

	    WHEN Dsid_party_idtype = "P"
		CALL "GET_XIDTYPE_FROM_SUFFIX" Using
		    By content "   "
		    By reference Dsid_id_ws
		    By Reference Dsid_id_ws_length
		    By Reference Dsid_shuffle_type
		  Returning Dsid_itsa_clearhouse

	    WHEN OTHER
		GO TO X960_SHUFFLE_IDS_END
	END-EVALUATE.

	If (Dsid_shuffle_parens_length = 0 )
	    %Beg
	    Dsid_compose ^OUT(Dsid_party_id)
		Dsid_shuffle_acct, "/", Dsid_shuffle_type, Dsid_shuffle_part1,
		Dsid_shuffle_oneof ( ^ONEOF ( (""),	
					      ("/AC"),
            		  		      ("/BC"),
            		  		      ("/CH"),
            		  		      ("/FW"),
            		  		      ("/CP"),
            		  		      ("/??"),
					      ("") ) ),

		Dsid_shuffle_part2, / ;
	    %End
	ELSE
	    %Beg
	    Dsid_compose ^OUT(Dsid_party_id)
		Dsid_shuffle_acct, "/", Dsid_shuffle_type, Dsid_shuffle_part1,
		Dsid_shuffle_oneof ( ^ONEOF ( (""),	
					      ("/AC"),
            		  		      ("/BC"),
            		  		      ("/CH"),
            		  		      ("/FW"),
            		  		      ("/CP"),
            		  		      ("/??"),
					      ("") ) ),
		Dsid_shuffle_part2, "/(", Dsid_shuffle_parens, / ;
	    %End	
	END-IF.
	Move Dsid_shuffle_idt to Dsid_party_idtype.

X960_SHUFFLE_IDS_END.
	EXIT.

X970_CHECK_FOR_IBAN.
	%^
	%beg dsid_temp_party_id = dsid_party_id; %end
	%^
	Call "VALIDATE_IBAN" Using
		by Reference dsid_temp_party_id
		by Reference dsid_temp_party_id_length
		by Reference dsid_iban_cntry_ws
		by Reference dsid_iban_bnk_ws
		by Reference dsid_iban_bnk_ws_length
		by Reference dsid_iban_brnch_ws
		by Reference dsid_iban_brnch_ws_length
		by Reference dsid_iban_curr_ws
		By Reference dsid_bnk_code_rec_ws
		By Reference dsid_bnk_code_rec_ws_lengths
		By Reference dsid_iban_valid_flag_ws
	returning dsid_val_iban_ret
	%^
	%^ Since we con;t care what party we are here for, We will set same flags
	%^ and let our caller determine the IBAN's fate This should only be present
	%^ for Credit or BNP. ID is customer accounts, not banks(right??)
	%^
	%^ 3 possible results Fail Pass and Fail passthru
	Evaluate  True	%^dsid_val_iban_ret
		When Failure_ignore_is   in dsid_val_iban_ret	%^ 2  %^ PASS/FAIL nothing for Us to do here
		When Failure_warning_is	 in dsid_val_iban_ret   %^ Fail       - possible Invalid Account
		When Failure_blocking_is in dsid_val_iban_ret
			%^ TODO -  Need to add new message indicating bad IBAN/BBAN
			%^
			Set Success_is in dsid_had_garbage to True
			%^
		When Success_is in dsid_val_iban_ret	%^ Pass       - Valid IBAN/BBAN
			%^ See if this is ON US, If so,
			%^ use it, otherwise, leave it be
			%^ WRITE THE NATIVE ACCOUNT HERE.
			%beg dsid_orig_iban_ws = dsid_party_id; %end
			Perform X980_WRITE_NATIVE_PRM thru
				X980_WRITE_NATIVE_PRM_END
			Set Failure_is in dsid_on_us_bnk_code to True
			Call "CUST_IS_ID_ON_US" using
				By Reference 	idacc of idkey of dsid_bnk_code_rec_ws
				By Reference 	idacc_length of dsid_bnk_code_rec_ws_lengths
				By Reference	dsid_bnk_code_bnk_ws
			Returning  dsid_on_us_bnk_code
			%beg ent_debit_set.flgs3.dbt_iban = "T"; %end
			%^ 117071
			If Not (( Cross_Bank_Search OF Menu_cfg = "T" OR
			          Cross_Bank_Search OF Menu_cfg = "Y" ))  AND
			   (dsid_bnk_code_bnk_ws NOT =  Bank of Loc_info of Ent_ftr_set)
			THEN
				%^ Cross Bank not allowed, so Push down
				Set Failure_is in dsid_on_us_bnk_code to True
			End-if
			If Success_is in dsid_on_us_bnk_code Then
			%^    If bank of loc_info of Ent_ftr_set = "QA3" And
			%^      Dsid_temp_party_id_length = 18
			%^    THen
			%^		%^ strip bank code
			%^		Move dsid_temp_party_id(5:14) to Dsid_gcheck_processed
			%^		Move 14 to Dsid_gcheck_processed_length
			%^		%beg dsid_compose ^out( dsid_temp_party_id) dsid_gcheck_processed,/; %end
			%^  end-if
			    If (Dsid_bnk_code_bnk_ws NOT = Bank of Loc_info of Ent_ftr_set)
			    Then
				%Beg    
		    		    dsid_compose ^OUT(dsid_gcheck_processed)
					dsid_bnk_code_bnk_ws, ":" dsid_temp_party_id,/;
			    	%End
			    Else
				%beg   dsid_gcheck_processed = dsid_temp_party_id;  %end
		  	    end-if
			end-if
			%^
			Set Failure_is in dsid_had_garbage to True %^ clear error if present
			%^ 
	End-evaluate.

X970_CHECK_FOR_IBAN_END.
	EXIT.
X980_WRITE_NATIVE_PRM.
	%^
	%Beg 
		  Compose ^OUT(dsid_prm_name_ws) "MTS$DBTNATIVE_ACCOUNT", /; 
		  Compose ^OUT(dsid_prm_value_ws) dsid_orig_iban_ws, /;
		  dsid_Prm_level_wo msg_is;
		  dsid_Prm_source_wo msg_is;
		  dsid_Prm_edit_wo Text_is;
	%End
	Initialize dsid_prm_time_on_ws, dsid_prm_time_off_ws
	Initialize dsid_prm_remaining_ws
	Set Success_is in dsid_prm_mode_wf to True
	Call "PRULE_MSG_UPDATE_PARAM" using
		   by reference dsid_prm_name_ws,	  %^ vstr(40)
		   by reference dsid_prm_name_ws_length,  %^ length
		   by reference dsid_prm_level_wo, 	  %^ PRULE_LEVEL_ONEOF.DDF
		   by reference dsid_prm_source_wo, 	  %^ PRULE_SOURCE_ONEOF.DDF
		   by reference dsid_prm_time_on_ws,	  %^ time
		   by reference dsid_prm_time_off_ws,	  %^ time
		   by reference dsid_prm_edit_wo, 	  %^ PR_PARAM_EDIT_ONEOF.DDF
		   by reference dsid_prm_mode_wf,	  %^ boolean
		   by reference dsid_prm_remaining_ws	  %^ long
		   by reference dsid_prm_value_ws,	  %^ vstr(80)
		   by reference dsid_prm_value_ws_length, %^ length
		   by reference dsid_prm_present_wf, 	  %^ boolean
		   by reference dsid_prm_memo_ws,	  %^ vstr(ACE$_MSG_STR_SIZE)
		   by reference dsid_prm_memo_ws_length	  %^ length
	   returning dsid_prm_status_wf.		  %^ boolean

X980_WRITE_NATIVE_PRM_END.
	EXIT.

%^******************************************************************************
%^
%^      DEBITSIDE_SCREEN
%^
%^******************************************************************************
%^
%^ Desired new features:
%^
%^	TODO - 
%^
%^
%^******************************************************************************
%^
%^ DEBITSIDE_SCREEN routine.
%^
%^   Calling format:
%^	Call "Debitside_screen" Using
%^		By Reference Debit_currency
%^		By Reference Xbank_account_ok
%^		By Reference Nochange_bank
%^		By Reference Debit_changed
%^		By Reference SBK_changed
%^		By Reference OBK_changed
%^		By Reference ORP_changed
%^		By Reference Account_changed
%^		By Reference Message_amount
%^              By Reference Message_currency
%^		By Reference Debit_account
%^		By Reference Is_payment
%^		By Reference Account_type
%^		By Reference Is_repetitive_lookup
%^		By Reference Lock_dbt_party
%^		By Reference Special_fee_key
%^		By Reference Debit_completed
%^		By Reference Debit_internal_state
%^		By Reference Currency_found
%^		By Reference Account_okay
%^		By Reference Nothing_suspicious
%^		By Reference Msg_bank_changed
%^		By Reference Error_Memo_count
%^	    Giving Return_status_ws.
%^	Possible return status values are:
%^		SUCCESS
%^		FAILURE
%^
%^	  Called to fill out debit side of message, retrieving any address
%^    information for on-file parties and to identify a debit account.  This
%^    is an interactive routine used by ENTRY/VFY/RPR/EXC and it is expected
%^    to be called multiple times as the operator creates and refines a
%^    message interactively.  "Changed" flags are used to indicate which
%^    debitside parties the operator has changed since the last time this
%^    routine was called for this message; any parties with a 0 changed
%^    flag are assumed to have been fully processed by a previous call
%^    to this routine.  If the debit party itself is changed we will not
%^    "roll back" any party insertions done when processing the previous
%^    debit party; that is left to the operator.                      
%^        We begin by processing the:
%^		 sending bank      (SBK)   SBK_changed
%^		 originator's bank (OBK)   OBK_changed
%^	    and	originating party  (ORP).  ORP_changed
%^    if their changed flag arguments are non-0.  (We will skip over any
%^    debitside party whose change flag is 0, assuming that it has been
%^    processed during a previous call to DEBITSIDE_SCREEN.)
%^        If we can find the SBK, OBK, and ORP address information, we copy it 
%^    into the message, overwriting any name and address information parsed
%^    from the message; we also set up the rel_id and adr_ptr elements.
%^    Addresses should not be ambiguous since RELGET performs address lookups
%^    and it will try to force the operator to resolve ambiguities.
%^	  Error messages are indicated by setting the appropriate field
%^    message sub-fields on the FTRSCR screen.  
%^	 If the debit party has changed, we will use RELGET to do an address
%^    lookup and we will set the Dbt_ovr flag appropriately -- "*" if the
%^    address is Not On File (NOF) and a SPACE if it is on-file.  This routine
%^    only resolves the initial debit party -- no AIN SI's will be executed
%^    here and no pushdowns or overrides will be done.
%^       We have now resolved the debit party address as best we can.  
%^    If the address is on the rel file, Ent_d_adr_set will be connected to it.
%^	 DEBITSIDE_SCREEN now proceeds to call its second half,
%^    DEBIT_SCREEN_ACCOUNT, using a reserved value of the debit internal status
%^    to execute any debit AINs and to choose a debit account.  Debit AIN's 
%^    can have an advice-type trigger, though we do not think that this 
%^    actually occurs in practise.  If a debit AIN with an advice-type trigger
%^    is encountered which would otherwise execute, AIN execution is suspended
%^    with that debit party and a provisional debit account and currency are 
%^    selected using the current debit address. The value of the debit internal
%^    status returned to DEBITSIDE_SCREEN (and passed back to its caller) 
%^    indicates whether AIN execution will need to be resumed on a second
%^    DEBIT_SCREEN_ACCOUNT call. 
%^        The DEBITSIDE_SCREEN routine will subsequently determine the advice
%^    method and call the DEBIT_SCREEN_ACCOUNT routine to process any remaining
%^    debit AIN SIs and set up the debit party address and account information.
%^	If any significant database anomalies were found in processing the
%^    debit party, the status is returned as FAILURE.  Otherwise it will be
%^    returned as SUCCESS.
%^      If the debit party lookup succeeded, the Nothing_suspicious argument
%^    will be returned non-0 to indicate that this message may continue
%^    through automated payment processing.
%^    
%^    
%^
%^ INPUT ARGUMENTS:
%^ explicit:
%^  Debit_currency	      STR(3)  contains the debit currency.
%^	This is a mandatory field.
%^  Xbank_account_ok	      boolean success if we are permitted to find a
%^	debit account in a different bank from the debit party.
%^  Nochange_bank	      boolean success if a debit party bank which 
%^	disagrees with the Menu_bnk_union bank should not cause a bank
%^	context switch.  (The Msg_bank_changed flag will still be hoisted
%^	when appropriate even though no context switch will be done.)
%^  Debit_changed	      Long   Non-0 if operator has changed the debit
%^	party since the last time we were called.
%^  Sbk_changed		      boolean   success if operator has changed the SBK party
%^	since the last time we were called.
%^  Obk_changed		      boolean   success if operator has changed the OBK party
%^	since the last time we were called.
%^  Orp_changed		      boolean   success if operator has changed the ORP party
%^	since the last time we were called.
%^  Account_changed	      boolean   success if caller has set or changed the
%^  	debit account override or debit account idtype since the last time 
%^ 	we were called.
%^  Message_amount	      DEC(14.2)  contains the message amount.
%^	This is a mandatory field.
%^  Message_currency          STR(3)  contains the message currency.  This
%^      is the currency trigger for SI's.
%^  Debit_account	      ACC_ID_REC.DDF contains account ID if caller
%^  	has pre-determined it; else spaces.
%^  Is_payment		      boolean   If success, message is a payment and 
%^	both an unambiguous debit address and debit account are required.
%^  Account_type	      STR(1) Select an account of this type from
%^ 	the accounts associated with the debit party address.
%^  Is_repetitive_lookup      Long   Non-0 if this is a repetitive lookup,
%^	in which case we will not map special instructions nor copy the
%^	debit account's cnf_seq to the message.
%^  Lock_dbt_party            Long   Non-0 if the debit party is locked
%^      (such as by instantiation of a locked repetitive) and should not be
%^ 	changed by AINs, INTRTL tables, or anything else.
%^ Special_fee_key	      Str(1) Passed to Set_debit_account.  
%^				     SPACE for nothing special.
%^				     For now, "W" if the fees are to be waived.
%^ implicit:
%^   Debit party:
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^   SBK:
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^   Menu_bnk_union.Id for current bank id.
%^
%^ OUTPUT ARGUMENTS:
%^explicit:
%^  Debit_completed           Long   Indicator of what state of completion the
%^	pre-debitside part of debitside-lookup achieved.
%^  Debit_internal_state      Long   Internal indicator of what state of 
%^	completion the pre-debitside part of debitside-lookup achieved.
%^  Currency_found            Str(3) is the debit currency found -- the
%^	explicit currency of the debit account, if any.  It will only be SPACES
%^	if no debit account was found.
%^  Account_okay 	      long   is returned 0 if any mapping errors
%^	were detected during debit account lookup, otherwise 1.
%^	Relget_msgcode contains detailed VMSG error message code.
%^  Nothing_suspicious	      long   is returned 0 if any mapping errors
%^	were detected during debitside lookup, otherwise 1.  For payments,
%^	mapping errors are caused by any ambiguous debitside parties, a
%^	NOF debit party, or a non-existent or ambiguous debit
%^	account.  For non-payments, mapping errors are caused by any
%^	ambiguous debitside party or a NOF (but not non-existent) debit
%^	party. If this flag is returned 0, the message should be routed to
%^	repair instead of being passed through for automated payment
%^	processing.
%^  Msg_bank_changed	      long   is returned non-0 if the debit party was
%^      found in a different bank, requiring the message (context) bank to
%^      change.
%^  Error_Memo_count          Long   is the number of error messages
%^	set on the FTRSCR during debitside lookup. Informational memos are
%^      written to the message history to indicate normal SI parsing and 
%^      execution; they are NOT counted in this total.
%^  Return_status_ws is SUCCESS if no obviously bad or inconsistent
%^      rel or aux file data was read during debitside lookup, otherwise 
%^      FAILURE.
%^
%^Implicit:
%^   Message history
%^   Debit party:
%^	Ent_debit_set.Dbt_adr_set_ptr
%^	Ent_debit_set.Dbt_rel_id
%^	Ent_debit_set.Dbt_adr_bnk_id
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_account
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^	Ent_debit_set.Dbt_acc_class
%^	Ent_debit_set.Dbt_acc_parent_code
%^	Ent_debit_set.Dbt_acc_prod_codes
%^	Ent_debit_set.Dbt_adr_class
%^	Ent_debit_set.Dbt_adr_type
%^	Ent_debit_set.Dbt_concen_acc
%^	Ent_debit_set.Dbt_currency
%^	Ent_debit_set.Dbt_department
%^	Ent_debit_set.Dbt_flag   
%^	Ent_debit_set.Dbt_recon_ref
%^	Ent_debit_set.Dbt_spc_inst1
%^	Ent_debit_set.Dbt_spc_inst2
%^	Ent_debit_set.Dbt_spc_inst3
%^	Ent_debit_set.Dbt_sys_of_rec
%^	Ent_debit_set.flgs.dbt_hold_flg
%^	Ent_debit_set.flgs.dbt_lim_flg  
%^	Ent_debit_set.flgs.dbt_ps_elig_flg
%^   SBK:
%^	Ent_debit_set.Sbk_adr_set_ptr
%^	Ent_debit_set.Sbk_adr_bnk_id
%^	Ent_debit_set.Sbk_rel_id
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk_adr_set_ptr
%^	Ent_debit_set.Obk_adr_bnk_id
%^	Ent_debit_set.Obk_rel_id
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp_adr_set_ptr
%^	Ent_debit_set.Orp_adr_bnk_id
%^	Ent_debit_set.Orp_rel_id
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^
%^
%^ Modification history:
%^
%^	Fred P. Isaacs	26-JUN-1995
%^		Initial version.
%^      Fred P. Isaacs  20-JUL-1995
%^		Now forces Dbt_adr_bnk_id to be Menu_bnk_union ID for NOF or
%^		ambiguous debit party and Cdt_adr_bnk_id to be same as
%^		Dbt_adr_bnk_id (or Menu_bnk_union ID for ENTRY if debit party
%^		bank is still blank).
%^      Fred P. Isaacs  31-JUL-1995
%^		Added Nochange_bank argument.
%^      Fred P. Isaacs  9-AUG-1995
%^              Made sure that Account_okay_ls argument flag is returned
%^		with proper setting.
%^		Made sure that credit account setup checks for cross-bank
%^		address/account relationship.
%^      Fred P. Isaacs  28-AUG-1995
%^              Tightened definition of full_parse for admins.  
%^      Fred P. Isaacs  8-SEP-1995
%^              Moved full_parse set logic to guaranteed exit path.
%^      Fred P. Isaacs  13-SEP-1995
%^		Flag null DBT_IDTYPE as a NOF.
%^      Fred P. Isaacs  26-OCT-1995
%^		Made Account_okay_ls default to TRUE when account change not
%^		done.
%^      Fred P. Isaacs  27-OCT-1995
%^              Adjusted Account_okay_ls to ONLY come up false if we should
%^		find an account and we don't.  (Party is on-file or account
%^		was passed in.)
%^	Fred P. Isaacs  12-JAN-1996
%^	     Posted V 4.0 fixes to V4.1:
%^              1)Changed lookup criteria for  CHIPS accounts. 
%^		2)Corrected over-enthusiastic setting of NOT fully parsed for
%^		  non-payment messages
%^	Fred P. Isaacs  8-FEB-1996
%^              Split off DEBIT_SCREEN_ACCOUNT routine to do final debit party
%^		lookup and account resolution.  Sequence of events is now
%^		to call DEBITSIDE_SCREEN to do subsidiary debitside parties
%^		and initial lookup of debit party; then to call
%^		CREDITSIDE_SCREEN to completely determine all creditside 
%^		parties; CREDITSIDE_SCREEN then calls DEBIT_SCREEN_ACCOUNT to
%^              finish up the debit party SIs and set the debit address and
%^		debit account information.
%^      Fred P. Isaacs  15-MAR-1996
%^		Posted V 4.0 changes -- eliminated bank id changes.
%^      Fred P. Isaacs  12-APR-1996
%^              Lower debitside parties now have AUX, not REL, lookups done.
%^      Fred P. Isaacs  19-APR-1996
%^		Now does not clear out debit name and address.
%^	Fred P. Isaacs	17-MAY-1996
%^              Shortened error memos; made error /info memo write more robust.
%^	Fred P. Isaacs	22-MAY-1996
%^		Changed args for SI_FIRST_DEBIT.
%^		Changed DEBITSIDE_SCREEN args to have separate amount and 
%^		   currency passed by caller.
%^	Fred P. Isaacs	31-MAY-1996
%^		Make sure that Relget_msgcode is cleared before each lookup
%^		step so we don't confuse an AUX lookup hit with a complete miss
%^      John R. Phelan  19-JUL-1996
%^              Sender Confirmations - If sender confirmations are activated,
%^              copy any sending bank instructions with a delivery flag of "Y"
%^              into the delivery standing instruction sequence.
%^      John R. Phelan  05-AUG-1996
%^              Check Dsid_ret_stat and Relget_msgcode coming back from
%^              RELGET, instead of just checking for a non-blank return key.
%^      John R. Phelan  30-AUG-1996
%^              Set the NOF Dbt_account correctly.  #19175, #19199, #19313.
%^      John R. Phelan  27-SEP-1996
%^              Prevent an ambiguous lookup "?", from being changed into
%^              a not-on-file lookup "*".  #19851
%^      Fred P. Isaacs  22-NOV-1996
%^              Move code to set/reset the Dbt_adr_bnk_id and the bank of
%^              loc_info of Ent_ftr_set back here BEFORE creditside lookup
%^              happens so bank context of rest of message will be correct.
%^		Migrated to 4.2 by John Phelan.
%^      John R. Phelan  30-DEC-1996
%^              Clear out debit party name and address when the address
%^              changes from on-file to not-on-file.  #21817
%^      John R. Phelan  23-JAN-1997
%^              Make sure address sets are always connected NOMOD.  #22302
%^      John R. Phelan  27-JAN-1997
%^              Add call(s) to SET_NOF_DBT_BNK_ID to determine not-on-file
%^              Dbt_adr_bnk_id based on various criteria.  #24397
%^      John R. Phelan  15-MAR-1997 
%^              Change the search currency when the debit party is changed by
%^              an AIN or is selected from a Relget or Rel_acc_from_adr
%^              screen.  #23745
%^      John R. Phelan  15-MAR-1997 
%^              Clear out the Dbt_hold flag from the previous account whenever
%^              the Dbt_account field is cleared.  #24455
%^      John R. Phelan  15-MAR-1997 
%^              Fix cross-currency argument to Rel_acc_from_adr.  #25675
%^
%^      Fred Kelley     02-Jun-1997   FUB #28375
%^              Added Dsid_use_bank argument to GET_ACCOUNT_CURRENCY to
%^              determine when to use bank defaults and when not to.
%^              4.1 to 4.2
%^
%^      John R. Phelan  04-Aug-1997
%^              Don't use the Relget Disp_id from the disambiguation screen to
%^              determine the new search currency.  #29028
%^
%^	Fred P. Isaacs  4-SEP-1997	SPR 28833
%^		Currency changes.
%^
%^      John R. Phelan  3-FEB-1998      
%^      35648   Changed to call the new Validate_pid_adv_type subroutine to
%^              determine the valid "P" source code, since this can now vary
%^              depending on bank.  This change is necessary for the Amex 
%^		German Clearing Interface.
%^
%^	Fred P. Isaacs  3-MAR-1998	40074
%^	Cleaned up setting of "completed" argument
%^
%^      John R. Phelan  13-APR-1998     41754
%^      Both Debitside_lookup and Debit_look_account were using the same
%^      field, Dsid_info_count.  Since Debitside_lookup can now call
%^      Debit_look_account directly, this was sometimes causing the
%^      info memo count to be doubled.  Debitside_lookup has been changed
%^      to use a field Dsid_adr_info_count and Debit_look_account uses
%^      Dsid_acc_info_count so they don't interfere with each other.
%^      Dsid_dbt_party_error has also been split into two fields for the
%^      same reason.
%^
%^	Fred P. Isaacs	17-APR-1998	#41577
%^	Corrected logic in check for whether to override debit party
%^	with Relget_return_key.
%^
%^	Fred P. Isaacs	23-APR-1998	#41577
%^	Corrected logic in check for whether to override debit party
%^	with Relget_return_key one more time.
%^
%^      John R. Phelan  30-JUN-1998     #43996
%^      Corrected logic in check for whether to override debit party
%^      with Relget_return_key.
%^
%^      John R. Phelan  22-JUL-1998     #43996
%^      Corrected logic (again) in check for whether to override debit party
%^      with Relget_return_key.
%^
%^	Ken Bjelke	29-Jun-1999	#49721 PORT
%^	Changed B260_DEBIT_PARTY to use SBK as relget_title to allow AUX lookup
%^	in rel_get when item is type xx33 Fed drawndown return.
%^                      
%^      Allen Joress    05-May-1999   spr 48541 - Migrate 42789
%^      Add code to distinguish if calling routine is from the PDQ_SERVER.
%^                         
%^	Fred P. Isaacs	14-Oct-1999	58119 for port
%^	PID suffix will be checked on suffixed PID against source;
%^	suffix for source will be appended onto unsuffixed PID.
%^
%^	Fred P. Isaacs	1-Nov-1999	58281 for port
%^	Initialize Currency_found_ls argument.
%^
%^ Ken Bjelke	12-Mar-2002	82215
%^	Check for function change from Rel_get, and exit if next func is different
%^	Indicates a timeout or a menu command.
%^
%^ Fred P. Isaacs  11-NOV-2002  90950
%^		Changed Prule_party_fsect to make AUX prules work.
%^
%^	Ken Bjelke 	11-Feb-2002	Spr 95109
%^	Correct setting of ORP returned ID_type from Rel_get.
%^
%^	Ken Bjelke 	18-Feb-2003	Spr 96089
%^	Ensure setting of ppp_Res_country fields for lower parties.
%^
%^ Ken Bjelke	05-Mar-2003   92807	
%^	Correct left over Country of residence when ID changes.
%^
%^ J. Walsh	05-Jun-2003	100909
%^	Make idtype and id fields return from REL_GET make it to screen.
%^
%^ T. Carroll	07-Aug-2003	101906
%^	Bypass debit party lookup when the party change flag is set to a 2.
%^	This means we a being called from RPR, EXC or VCH and the party is
%^	already present.
%^
%^
%^^ Ken Bjelke 	21-Aug-2003	103527	
%^	Clear res_country of DBT if the ent_debit  and the change
%^	set are the same. but the party has changed. This should handle 
%^	most cases.
%^
%^ C. Crain	30-Dec-2003	97347
%^	Set adr_set_ptr for ORP, OBK and SBK when address is in AUX and
%^	flgs3 for ORP, OBK and SBK.  
%^
%^ Ken Bjelke  22-Jan-2004	109226
%^	set adr pointers for AUX addresses needed for AIN detection
%^
%^ J. Walsh	27-Jan-2004	109364
%^	Change test for MENU command after return from REL_GET.
%^
%^ J. Walsh	28-Jan-2004	SPR 109405
%^	Do not allow debit party to change after a call to REL_GET if the
%^	field is protected on the ftrscr.
%^
%^ C. Crain	13-Feb-2004	PER 97347
%^	For Lower debit parties, replace "E" Idtype BCC with BIC based on
%^	LKUP_BIC_BY_BCC flag.
%^
%^ J. Walsh	01-Mar-2004     SPR 110546
%^    	Add function of RPR and debit idtype of A to tests for 109405
%^
%^ Ken Bjelke 	04-Apr-2004 	109679
%^	Don't set debitside_screen_status to failure on SWIFT_ADR_NOF_WARN
%^	check. This was resulting in the Debit Party lookup results being incorrectly
%^	evaluated. Bogus_party counter was already being incremented, this works correctly.
%^
%^	J. Walsh	23-Sep-2004	113566
%^	Test relget_reject_flag for M: user executed a MENU command in REL_GET.
%^
%^	S. Smith	22=Apr-2005	Spr 122578
%^	Null out the debit prod_code if account changes, also dbt_account
%^	and acc_class.
%^
%^	S. Smith	4-Nov-2005	Spr 127007
%^	Do not skip the end of b240_sbk if the sbk_idtype is blank, we need 
%^	to fall thru and null out the dbt_rel_id so dbt_auth_check will work
%^	correctly
%^
%^ Ken Bjelke 	21-Jul-2006	128654
%^	Correct INS lookup check. Was not detecting changed field.
%^
%^ R. Beer		12-Dec-2007	CR648, SPR 140521
%^	Unable to release transaction that was flagged for not-on-file debit
%^	party after new address and account records were added.
%^
%^ Ken Bjelke		19-Aug-2008	CR=3145, CR3771
%^	Correct on AUX debit party, should remain ovr "*" and NOF.
%^	This corrects ftrscr from adding a Bank Prefix to the CDT and Sender.
%^
%^ Ken Bjelke		20-Aug-2008	CR3283 - CR 3773
%^	Only clear the dbt_res_country when the res = the change_set and
%^	the dbt_typ does not = the change_set.
%^
%^ J. Carey	10-Feb-2009	CR6612 - Migrate CR5935 from version 2.0
%^	Problem:  MTS sent SWF wire with a BEI in field 52A.
%^	That is prohibited.  The ID in field 52A cannot be a BEI.
%^	Explanation:  In certain cases, MTS is not identifying
%^	that a BIC is really a BEI.  Need to update the code
%^	that populates the BEI Flags for the debit parties
%^	(SBK_bei_flag, ORP_bei_flag, and OBK_bei_flag).
%^	This code was not cognizant of all the Swf subtypes that
%^	indicate a BEI.  The SWIFT handbook for 2008 lists
%^	six Swift subtypes that that a BIC is really a BEI:
%^	    BEID, CORP, MCCO, SMDP, TESP, TRCO
%^
%^ J. Pfaff     12-Feb-2009     CR7795
%^      Add setting cst_party_usage.
%^
%^ End Revision History
%^******************************************************************************
%module DEBITSIDE_SCREEN;

%^*****************************************************************************
%^                                                                            *
%^ Paragraphs in this procedure are labeled as follows:                       *
%^                                                                            *
%^	A100_MAIN	     Subroutine entry point, dispatch, and return
%^      B200_ORP	     Looks up ORiginating Party.
%^      B220_OBK	     Looks up Originating BanK.
%^      B240_SBK	     Looks up Sending BanK.
%^      B260_DEBIT_PARTY     Looks up Debit Party and sets up message.
%^	C440_COPY_ADDRESS    Copies address info into debit party.
%^	D100_BCC_LKUP_BIC    Replaces BCC with BIC based on config	
%^                                                                            *
%^                                                                            *
%^*****************************************************************************
%^ Macro definitions.
%^ %MAR
%^ .default	displacement,long
%^ %end

%^*****************************************************************************
%linkage
01  Debit_currency_ls	  	%Str(3);
01  Xbank_account_ok_ls	  	%boolean;
01  Nochange_bank_ls	  	%boolean;
01  Debit_changed_ls	  	%Long;
01  SBK_changed_ls	  	%boolean;
01  OBK_changed_ls	  	%boolean;
01  ORP_changed_ls	  	%boolean;
01  Account_changed_ls	  	%boolean;
01  Message_amount_ls	  	%Amount;
01  Message_currency_ls   	%Str(3);
01  Debit_account_ls	  	%Str(34);
01  Is_payment_ls	  	%boolean;
01  Account_type_ls 	  	%Str(1);
01  Is_rptv_lookup_ls	  	%boolean;
01  Lock_dbt_party_ls	  	%boolean;
01  Special_fee_key_ls	  	%Str(1);
01  Debit_completed_ls	  	%Long;
01  Internal_state_ls	  	%Long;
01  Currency_found_ls 	  	%Str(3);
01  Account_okay_ls 	  	%boolean;
01  Nothing_suspicious_ls 	%boolean;
01  Msg_bank_changed_ls	  	%boolean;
01  Error_memo_count_ls	  	%Long;
01  Debitside_screen_status_ls	%Boolean;

%Procedure using Debit_currency_ls, Xbank_account_ok_ls, Nochange_bank_ls, 
		Debit_changed_ls, SBK_changed_ls, OBK_changed_ls,
		ORP_changed_ls, Account_changed_ls, Message_amount_ls,
		Message_currency_ls, Debit_account_ls, Is_payment_ls,
		Account_type_ls, Is_rptv_lookup_ls, Lock_dbt_party_ls,
		Special_fee_key_ls, Debit_completed_ls, Internal_state_ls,
		Currency_found_ls, Account_okay_ls, Nothing_suspicious_ls,
		Msg_bank_changed_ls, Error_memo_count_ls 
	RETURNING  Debitside_screen_status_ls.
%^*****************************************************************************

%^*****************************************************************************
A100_MAIN.
*  Initialize returned variables.
%^
%^ Universal Reset
	Set Failure_is in Resolve_ambiguous to True.

	Move Debit_completed_ls to Dbt_screen_State.
        Set Debit_is in cst_party_usage to True.

	Perform Until Success_is in resolve_ambiguous  Or
		      Complete_is in Dbt_screen_state Or
		      Error_is    in Dbt_screen_state

	    EVALUATE True

		When	Init_is in Dbt_screen_State
			 Perform B100_DBT_SCREEN_INIT Thru
				 B100_DBT_SCREEN_INIT_END

		When Orp_Lookup in Dbt_screen_State
			Perform B200_ORP_LOOKUP Thru B200_ORP_LOOKUP_END

		When Orp_index_conn in Dbt_screen_State
			Perform B200_ORP_CONN   Thru B200_ORP_CONN_END

		When Orp_map in Dbt_screen_State
			Perform B200_ORP_MAP	Thru B200_ORP_MAP_END


		When Obk_Lookup in Dbt_screen_State
			Perform B220_OBK_LOOKUP Thru B220_OBK_LOOKUP_END

		When Obk_index_conn in Dbt_screen_State
			Perform B220_OBK_CONN   Thru B220_OBK_CONN_END

		When Obk_map in Dbt_screen_State
			Perform B220_OBK_MAP	Thru B220_OBK_MAP_END

		When Ins_Lookup in Dbt_screen_State
			Perform B230_INS_LOOKUP Thru B230_INS_LOOKUP_END

		When Ins_index_conn in Dbt_screen_State
			Perform B230_INS_CONN   Thru B230_INS_CONN_END

		When Ins_map in Dbt_screen_State
			Perform B230_INS_MAP	Thru B230_INS_MAP_END

		When Sbk_Lookup in Dbt_screen_State
			Perform B240_SBK_LOOKUP Thru B240_SBK_LOOKUP_END

		When Sbk_Lookup2 in Dbt_screen_State
			Perform B240_SBK_LOOKUP2 Thru B240_SBK_LOOKUP2_END

		When  Sbk_index_conn1 in Dbt_screen_State
			Perform B240_SBK_INDEX_CONN1 Thru B240_SBK_INDEX_CONN1_END

		When Sbk_index_conn2 in Dbt_screen_State
			Perform B240_SBK_INDEX_CONN2 Thru B240_SBK_INDEX_CONN2_END

		When Sbk_map in Dbt_screen_State
			Perform B240_SBK_MAP	Thru B240_SBK_MAP_END

		When Dbt_Lookup1 in Dbt_screen_State
			Perform B260_DBT_LOOKUP1 Thru B260_DBT_LOOKUP1_END

		When Dbt_index_conn1 in Dbt_screen_State
			Perform B260_DBT_INDEX_CONN1   Thru B260_DBT_INDEX_CONN1_END

		When Dbt_Lookup2 in Dbt_screen_State
			Perform B260_DBT_LOOKUP2 Thru B260_DBT_LOOKUP2_END

		When Dbt_index_conn2 in Dbt_screen_State
			Perform B260_DBT_INDEX_CONN2   Thru B260_DBT_INDEX_CONN2_END

		When Dbt_map in Dbt_screen_State
			Perform B260_DBT_MAP	Thru B260_DBT_MAP_END

	  End_Evaluate

	End-Perform.

	If Success_is in Resolve_Ambiguous
	Then
		%^ Exit Program
		Move Dbt_screen_state to Debit_completed_ls
		Go to A100_MAIN_END
	End-if.


A100_MAIN_EXIT.
* See if we can continue on to call DEBIT_SCREEN_ACCOUNT
	%Beg
	Dsid_internal DSTATE_INIT ;
	Dsid_intern_word = Dsid_internal ;
	%End
	Move Dsid_intern_word to Internal_state_ls
	If (Success_is in Debitside_screen_status_ls   )
	    Move Debit_currency_ls to Dsid_acct_currency
	    Call "DEBIT_SCREEN_ACCOUNT" USING
		By Reference Dsid_acct_currency
		By Reference Debit_currency_ls
		By Reference Message_amount_ls
		By Reference Message_currency_ls
		By Reference Debit_account_ls
		By Reference Is_payment_ls
		By Reference Xbank_account_ok_ls
		By Reference Account_type_ls
		By Reference Is_rptv_lookup_ls
		By Reference Nochange_bank_ls
	 	By Reference Lock_dbt_party_ls
		By Reference Special_fee_key_ls
		By Reference Account_changed_ls
		By Reference Debit_changed_ls
		By Reference Internal_state_ls
		By Reference Currency_found_ls
		By Reference Account_okay_ls
		By Reference Dsid_acct_nosusp
		By Reference Dsid_acc_bank_change
		By Reference Dsid_acct_errmemos
	      RETURNING Dsid_acct_status
	    If Dsid_acc_bank_change NOT = 0 
		set Success_is in Msg_bank_changed_ls to true
	    END-IF	
	    If Failure_is in Dsid_acct_status  
		Set Failure_is in Debitside_screen_status_ls to true
	    END-IF
	    Add Dsid_acct_errmemos to Error_memo_count_ls
	ELSE
	    Move ZERO to Dsid_acct_errmemos
	    Move ZERO to Dsid_acct_nosusp
	END-IF.    	    	

	If (Dsid_bogus_parties = 0 )
	   AND (Dsid_acct_nosusp = 1 )
	THEN
* Debit party was completely processed.
	    Set Success_is in Nothing_suspicious_ls to true
	END-IF.

	%Beg
	BREAK: Dsid_acc_seq ;
	%End.

	Move Internal_state_ls to Dsid_internal
        If (DSTATE_INIT in Dsid_internal )
	   AND (Dbt_ovr of Ent_debit_set NOT = SPACES )
	THEN
	    Call "SET_NOF_DBT_ACCOUNT"
	    %Beg
	    Dsid_internal DSTATE_DONE ;
	    Dsid_intern_word = Dsid_internal ;
	    %End
	    Move Dsid_intern_word to Internal_state_ls
        END-IF.

%^	If (DSTATE_HOLD in Dsid_internal )
%^	    Move ZERO to Debit_completed_ls
%^	ELSE
%^	    Move 1 to Debit_completed_ls
%^	END-IF.

        Set Init_is in Dbt_screen_state to True.
	Move dbt_screen_state to Debit_completed_ls.

A100_MAIN_END.
        %EXIT PROGRAM.


B100_DBT_SCREEN_INIT.
	Set Success_is in Debitside_screen_status_ls to true.
	Set Failure_is in dsid_lower_party_erred to True.
	Set Failure_is in Nothing_suspicious_ls to true.
	Move ZERO to Error_memo_count_ls.
	Set Failure_is in Msg_bank_changed_ls to true.
	Move ZERO to Dsid_bogus_parties.
        Move Debit_currency_ls to Dsid_currency_ws.
	Move SPACES to Currency_found_ls.
	Move spaces to Debit_account_ls.	%^ Only used by lower routines set there.
	
*  Set up local flags.
	Set Failure_is in Dsid_set_address to TRUE.
	Set Failure_is in Dsid_debit_pend_del to TRUE.


%^ Get config flag for replacing lower debit party BCC Id with BIC

	Call "GCV_LKUP_BIC_BY_BCC" using
		by reference Lkup_bic_by_bcc_ws.

	Set Orp_lookup in Dbt_screen_state to True.

B100_DBT_SCREEN_INIT_END.
	EXIT.


B200_ORP_LOOKUP.
%^
	If (Failure_is in Orp_changed_ls)	%^ No change
	Then
		Set OBK_LOOKUP in Dbt_screen_state to True
		Go to B200_Orp_Lookup_End
	End-if.
%^
*  Paragraph to do account lookup on the ORP; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	If (orp_adr_ptr_ok of flgs3 of ent_debit_set NOT = Spaces)
	then	%^ hose out prior lookup
	    %beg
	    Ent_debit_set (.orp_rel_id     =  <0>,
			   .orp_adr_set_ptr DELETE,
			   .flgs3.orp_adr_ptr_ok = NULL ) ;
	    %end
	end_if.

	Move ZERO to Relget_msgcode.
* put in N or @ id if left blank by operator
	IF (Orp_idtype of Orp of Ent_debit_set = "N" OR "@")
	   AND (Orp_id of Orp of Ent_debit_set = SPACES)
	THEN
	    If Orp_name1 of Ent_debit_set NOT = SPACES
		MOVE SPACES TO Dsid_20charid_ws
		If Orp_idtype OF Orp OF Ent_debit_set = "N"
		    Call "AUTONAM" USING
			By reference Orp_name1 OF Ent_debit_set
			By reference Orp_name1_length of Ent_debit_set_lengths
			By reference Dsid_20charid_ws
			By reference Dsid_length
		ELSE
		    Call "CREATE_AUX_MNAM" USING 
			By reference Dsid_20charid_ws
			By reference Orp_name1_length of Ent_debit_set_lengths
			By reference Orp_name1 OF Ent_debit_set
			By reference Orp_name2_length OF Ent_debit_set_lengths
			By reference Orp_name2 OF Ent_debit_set
			By reference Orp_name3_length OF Ent_debit_set_lengths
			By reference Orp_name3 OF Ent_debit_set
			By reference Orp_name4_length OF Ent_debit_set_lengths
			By reference Orp_name4 OF Ent_debit_set
		END-IF
		MOVE %SIZ(Dsid_20charid_ws) TO Dsid_20charid_ws_length
		%Beg
		Dsid_compose ^OUT(Ent_debit_set.Orp.Orp_id), 
				Dsid_20charid_ws, /;
		%End
	    ELSE
*		error, name missing
%^		%Beg Ftrscr.debit.orp_name1.msg = "FTRSCR$_NAMEMISSING"; %End
		%beg
			field_ws = "DEBIT.ORP_NAME1";
			Mnemonic_ws = "FTRSCR$_NAMEMISSING" ;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Add 1 to Dsid_bogus_parties
		Add 1 to Error_memo_count_ls
		Set OBK_LOOKUP in Dbt_screen_state to True
		GO TO B200_Orp_Lookup_End
	    END-IF
	END-IF.

* no lookup if no id type
	If Orp_idtype of Orp of Ent_debit_set = SPACE
	    Set OBK_LOOKUP in Dbt_screen_state to True
	    GO TO B200_Orp_Lookup_End
	END-IF.
	%Beg
	Dsid_parse ^IN(Ent_debit_set.Orp.Orp_id ) ^SPACE, Dsid_id_ws, 
		^SPACE, / ;
	%End.

	Set ORP in Relget_title_flag to TRUE.
* Force the SWF lookup into the AUX database ONLY.
	If Orp_idtype of Orp of Ent_debit_set = "S"
	    Move "$" to Dsid_temp_idtype
	ELSE
	    Move Orp_idtype of Orp of Ent_debit_set To Dsid_temp_idtype
	END-IF.

	%Beg  Dsid_next_function = Menu_next_function;  %End
	%Beg  Menu_arg_cmd_vstr = "";  %End.
	Move SPACE to Dsid_lkup_pend_del.
	Call "REL_GET" USING 
	    by reference Dsid_temp_idtype
	    By reference Dsid_id_ws
	    By reference Dsid_id_ws_length
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	If Success_is in Relget_ambiguous_wf 
	Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Orp_Index_Conn 	in Dbt_screen_state  to True
		Go to B200_ORP_LOOKUP_END
	end-if.

	%^ NEED a better exit stategy
	%^
	If Relget_reject_flag = "M"
	    Go to B200_orp_lookup_end
	End-if.

	If dsid_next_function  not = Menu_next_function Then
		Go to B200_orp_lookup_end
	end-if.
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
		Set Menu_is in Dbt_screen_state to True
	    Go to B200_orp_lookup_end
	End-if.

	Set Orp_map in Dbt_screen_state to True.

B200_ORP_LOOKUP_END.
	EXIT.

B200_ORP_CONN.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.
%^
	Set ORP_Map in Dbt_screen_state to True.
%^
B200_ORP_CONN_END.
	EXIT.


B200_ORP_MAP.
        If (Success_is in Dsid_ret_stat  )
	   OR (Relget_msgcode = Vmsg_dat_notonfile_wc )
        THEN
* We actually did get a REL or AUX db hit.
* Move RELGET's returned id into a vstr for checking.
	    %Beg
	    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
	    %End
	    Move "ORP" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		GO TO B200_ORP_MAP_CLEANUP
	    end-if
	ELSE
	    %Beg  Dsid_return_key = NULL ;  %End
	END-IF.

* If the IDTYPE or ID was changed by RELGET, move in the returned key
        If (Dsid_return_key NOT = spaces and Dsid_return_key_length NOT = 0)
            If (Relget_return_idtype NOT = Orp_idtype of Orp of Ent_debit_set)
            or (Dsid_return_key_length NOT = Dsid_id_ws_length)
            or (Dsid_return_key(1:Dsid_id_ws_length) NOT = Dsid_id_ws(1:Dsid_id_ws_length))
            THEN
* Pick up any changes that Relget made to the idtype or id.
		%Beg
		Ent_debit_set.Orp(
		    .Orp_idtype = Relget_return_idtype,
		    .Orp_id = Dsid_return_key);
		%End
           END-IF
	END-IF.

	If (Failure_is in Dsid_ret_stat   )
%^	    Move ZERO to Dsid_found_it
	    Set Failure_is in Dsid_found_it to True
	    If (Orp_rel_id of Ent_debit_set NOT = 0 )
* Pre-owned ORP, reset by user.  Flush it.
		%Beg
		Ent_debit_set (.orp_rel_id     =  <0>,
			       .orp_adr_set_ptr DELETE,
			       .flgs3.orp_adr_ptr_ok = NULL ) ;
%^		Dsid_found_it = <1> ;
		%End
		Set Success_is in Dsid_found_it to TRUE
	    END-IF
	    If (Relget_msgcode NOT = Vmsg_dat_notonfile_wc )
%^		If (Dsid_found_it NOT = 0 )
		If (Success_is in Dsid_found_it)
* Current ORP address was from a previous lookup
		    %Beg
%^ Clear out any lingering information.
		    Ent_debit_set (.orp_name1      = NULL,
			           .orp_name2      = NULL,
			           .orp_name3      = NULL,
			           .orp_name4      = NULL,
			           .orp_adr_bnk_id = NULL,
				   .orp_res_country = NULL ) ;
		    Ent_debit_set.orp_bei_flag = Null;
		    %End
		END-IF
	        If (Orp_idtype of Orp of Ent_debit_set = "S")
	           AND (Swift_adr_nof_warning of Menu_cfg = "T")
	        THEN
%^18-May-89 For SWIFT ID's, set fatal error if INTRTL flag is set
%^		    %Beg Ftrscr.debit.orp.orp_id.msg = "VMSG$_LOOKUPFAIL";   %End
		    %beg
			field_ws = "DEBIT.ORP.ORP_ID";
			Mnemonic_ws = "VMSG$_LOOKUPFAIL" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		    Add 1 to Dsid_bogus_parties
		    Add 1 to Error_memo_count_ls
 		    Set Success_is in dsid_lower_party_erred to True
		    %^Set Failure_is in Debitside_screen_status_ls to true
	        END-IF
	        GO TO B200_ORP_MAP_CLEANUP
	    END-IF
	END-IF.

%^ We now have at least copy-able ORP info.
	%Beg
	Ent_debit_set (.Orp_name1 = Relget_adr_set.adr_name,
		       .Orp_name2 = Relget_adr_set.adr1,
		       .Orp_name3 = Relget_adr_set.adr2,
		       .Orp_name4 = Relget_adr_set.adr3,
		       .Orp_adr_bnk_id = Relget_adr_set.bnk_id) ;
	%End.

%^ cr6612 - cannot implement cr6612 without setting the bei flag
	If Orp_idtype of Orp of Ent_debit_set = "S" Then
		If swf_subtype of relget_adr_set  = "BEID" or "CORP" or "MCCO" or "SMDP" or "TESP" or "TRCO" Then
			%beg  ent_debit_set.orp_bei_flag = "Y"; %end
		else
			%beg  ent_debit_set.orp_bei_flag = Null; %end
		end-if
	end-if.

	If (Rel_id of Relget_adr_set NOT = ZERO )
	    %Beg
	    Ent_debit_set (.Orp_rel_id = Relget_adr_set.rel_id,
			   .Orp_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.orp_adr_ptr_ok = "T" ) ;
	    %End
	Else
	    %^ This must be an AUX hit, need to set the Pointer
	   %beg
	    Ent_debit_set (.Orp_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.orp_adr_ptr_ok = "T" ) ;
	   %end
	END-IF.

*
* If there's a zip code, call subroutine to put it into address
*
	IF (Zip of Relget_adr_set NOT = SPACES ) THEN
	    Call "ZIPSUB" USING
		BY REFERENCE Orp_name4 of Ent_debit_set
		BY REFERENCE Orp_name4_length of Ent_debit_set_lengths
		BY REFERENCE Orp_name3 of Ent_debit_set
		BY REFERENCE Orp_name3_length of Ent_debit_set_lengths
		BY REFERENCE Zip OF Relget_adr_set,
		BY REFERENCE Line_flg_ws
	    If (Line_flg_ws = "3" ) 
		%Beg  Ent_debit_set.Orp_name3 CHANGE ;  %End
	    ELSE
		%Beg  Ent_debit_set.Orp_name4 CHANGE ;  %End
	    END-IF
	END-IF.

B200_ORP_MAP_CLEANUP.

	If orp_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "ORP"
	       by reference orp_idtype of Ent_debit_set
	       by reference orp_id of Ent_debit_set
	       by reference orp_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference orp_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.orp_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

        %Beg  BREAK: Relget_adr_set ;  %End.

	Set Obk_LOOKUP in Dbt_screen_state to True.	

B200_ORP_MAP_END.
	EXIT.


B220_OBK_LOOKUP.
%^
	If (Failure_is in Obk_changed_ls)	%^ No change
	Then
		Set Ins_LOOKUP in Dbt_screen_state to True
		Go to B220_Obk_Lookup_End
	End-if.
%^
*  Paragraph to do account lookup on the OBK; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	If (obk_adr_ptr_ok of flgs3 of ent_debit_set NOT = Spaces)
	then	%^ hose out prior lookup
	    %beg
	    Ent_debit_set (.obk_rel_id     =  <0>,
			   .obk_adr_set_ptr DELETE,
			   .flgs3.obk_adr_ptr_ok = NULL ) ;
	    %end
	end_if.

	Move ZERO to Relget_msgcode.
* put in N or @ id if left blank by operator
	IF (Obk_idtype of Obk of Ent_debit_set = "N" OR "@")
	   AND (Obk_id of Obk of Ent_debit_set = SPACES)
	THEN
	    If Obk_name1 of Ent_debit_set NOT = SPACES
		MOVE SPACES TO Dsid_20charid_ws
		If Obk_idtype OF Obk OF Ent_debit_set = "N"
		    Call "AUTONAM" USING
			By reference Obk_name1 OF Ent_debit_set
			By reference Obk_name1_length of Ent_debit_set_lengths
			By reference Dsid_20charid_ws
			By reference Dsid_length
		ELSE
		    Call "CREATE_AUX_MNAM" USING 
			By reference Dsid_20charid_ws
			By reference Obk_name1_length of Ent_debit_set_lengths
			By reference Obk_name1 OF Ent_debit_set
			By reference Obk_name2_length OF Ent_debit_set_lengths
			By reference Obk_name2 OF Ent_debit_set
			By reference Obk_name3_length OF Ent_debit_set_lengths
			By reference Obk_name3 OF Ent_debit_set
			By reference Obk_name4_length OF Ent_debit_set_lengths
			By reference Obk_name4 OF Ent_debit_set
		END-IF
		MOVE %SIZ(Dsid_20charid_ws) TO Dsid_20charid_ws_length
		%Beg
		Dsid_compose ^OUT(Ent_debit_set.Obk.Obk_id), 
				Dsid_20charid_ws, /;
		%End
	    ELSE
*		error, name missing
%^		%Beg Ftrscr.debit.obk_name1.msg = "FTRSCR$_NAMEMISSING"; %End
		%beg
			field_ws = "DEBIT.OBK.NAME1";
			Mnemonic_ws = "FTRSCR$_NAMEMISSING" ;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Add 1 to Dsid_bogus_parties
		Add 1 to Error_memo_count_ls
		Set Ins_LOOKUP in Dbt_screen_state to True
		GO TO B220_Obk_Lookup_End
	    END-IF
	END-IF.

* no lookup if no id type
	If Obk_idtype of Obk of Ent_debit_set = SPACE
  	    Set Ins_LOOKUP in Dbt_screen_state to True
	    GO TO B220_Obk_Lookup_End
	END-IF.
	%Beg
	Dsid_parse ^IN(Ent_debit_set.Obk.Obk_id ) ^SPACE, Dsid_id_ws, 
		^SPACE, / ;
	%End.

	Set OBK in Relget_title_flag to TRUE.
* Force the SWF lookup into the AUX database ONLY.
	If Obk_idtype of Obk of Ent_debit_set = "S"
	    Move "$" to Dsid_temp_idtype
	ELSE
	    Move Obk_idtype of Obk of Ent_debit_set To Dsid_temp_idtype
	END-IF.

	%Beg  Dsid_next_function = Menu_next_function;  %End
	%Beg  Menu_arg_cmd_vstr = "";  %End.
	Move SPACE to Dsid_lkup_pend_del.
	Call "REL_GET" USING 
	    by reference Dsid_temp_idtype
	    By reference Dsid_id_ws
	    By reference Dsid_id_ws_length
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.


	If Success_is in Relget_ambiguous_wf 
	Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Obk_Index_Conn 	in Dbt_screen_state  to True
		Go to B220_OBK_LOOKUP_END
	end-if.

	%^ NEED a better exit stategy
	%^
	If Relget_reject_flag = "M"
	    Go to B220_obk_lookup_end
	End-if.

	If dsid_next_function  not = Menu_next_function Then
		Go to B220_obk_lookup_end
	end-if.
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
		Set Menu_is in Dbt_screen_state to True
	    Go to B220_obk_lookup_end
	End-if.

	Set Obk_map in Dbt_screen_state to True.

B220_OBK_LOOKUP_END.
	EXIT.

B220_OBK_CONN.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.
%^
	Set OBK_Map in Dbt_screen_state to True.
%^
B220_OBK_CONN_END.
	EXIT.


B220_OBK_MAP.

%^	Assume that we will continue
	Set SBK_Lookup in Dbt_screen_state to True.	

        If (Success_is in Dsid_ret_stat  )
	   OR (Relget_msgcode = Vmsg_dat_notonfile_wc )
        THEN
* We actually did get a REL or AUX db hit.
* Move RELGET's returned id into a vstr for checking.
	    %Beg
	    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
	    %End
	    Move "OBK" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		GO TO B220_OBK_MAP_CLEANUP
	    end-if
	ELSE
	    %Beg  Dsid_return_key = NULL ;  %End
	END-IF.

* If the IDTYPE or ID was changed by RELGET, move in the returned key
        If (Dsid_return_key NOT = spaces and Dsid_return_key_length NOT = 0)
            If (Relget_return_idtype NOT = Obk_idtype of Obk of Ent_debit_set)
            or (Dsid_return_key_length NOT = Dsid_id_ws_length)
            or (Dsid_return_key(1:Dsid_id_ws_length) NOT = Dsid_id_ws(1:Dsid_id_ws_length))
            THEN
* Pick up any changes that Relget made to the idtype or id.
		%Beg
		Ent_debit_set.Obk(
		    .Obk_idtype = Relget_return_idtype,
		    .Obk_id = Dsid_return_key);
		%End
           END-IF
	END-IF.

	If (Failure_is in Dsid_ret_stat   )
%^	    Move ZERO to Dsid_found_it
	    Set Failure_is in Dsid_found_it to True
	    If (Obk_rel_id of Ent_debit_set NOT = 0 )
* Pre-owned OBK, reset by user.  Flush it.
		%Beg
		Ent_debit_set (.obk_rel_id     =  <0>,
			       .obk_adr_set_ptr DELETE,
			       .flgs3.obk_adr_ptr_ok = NULL ) ;
%^		Dsid_found_it = <1> ;
		%End
		Set Success_is in Dsid_found_it to TRUE
	    END-IF
	    If (Relget_msgcode NOT = Vmsg_dat_notonfile_wc )
%^		If (Dsid_found_it NOT = 0 )
		If (Success_is in Dsid_found_it)
* Current OBK address was from a previous lookup
		    %Beg
%^ Clear out any lingering information.
		    Ent_debit_set (.obk_name1      = NULL,
			           .obk_name2      = NULL,
			           .obk_name3      = NULL,
			           .obk_name4      = NULL,
			           .obk_adr_bnk_id = NULL,
				   .obk_res_country = NULL ) ;
 		    Ent_debit_set.obk_bei_flag = Null;
		    %End
		END-IF
	        If (Obk_idtype of Obk of Ent_debit_set = "S")
	           AND (Swift_adr_nof_warning of Menu_cfg = "T")
	        THEN
%^18-May-89 For SWIFT ID's, set fatal error if INTRTL flag is set
%^		    %Beg   Ftrscr.debit.obk.obk_id.msg = "VMSG$_LOOKUPFAIL"; %End
		    %beg
			field_ws = "DEBIT.OBK.OBK_ID";
			Mnemonic_ws = "VMSG$_LOOKUPFAIL" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		    Add 1 to Dsid_bogus_parties
		    Add 1 to Error_memo_count_ls
 		    Set Success_is in dsid_lower_party_erred to True
		    %^Set Failure_is in Debitside_screen_status_ls to true
	        END-IF
	        GO TO B220_OBK_MAP_CLEANUP
	    END-IF
	END-IF.

%^ We now have at least copy-able OBK info.
	%Beg
	Ent_debit_set (.Obk_name1 = Relget_adr_set.adr_name,
		       .Obk_name2 = Relget_adr_set.adr1,
		       .Obk_name3 = Relget_adr_set.adr2,
		       .Obk_name4 = Relget_adr_set.adr3,
		       .Obk_adr_bnk_id = Relget_adr_set.bnk_id) ;
	%End.

%^ cr6612 - cannot implement cr6612 without setting the BEI flag.
	If Obk_idtype of Obk of Ent_debit_set = "S" Then
		If swf_subtype of relget_adr_set  = "BEID" or "CORP" or "MCCO" or "SMDP" or "TESP" or "TRCO" Then
			%beg  ent_debit_set.obk_bei_flag = "Y"; %end
		else
			%beg  ent_debit_set.obk_bei_flag = Null; %end
		end-if
	end-if.

	If (Rel_id of Relget_adr_set NOT = ZERO )
	    %Beg
	    Ent_debit_set (.Obk_rel_id = Relget_adr_set.rel_id,
			   .Obk_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.obk_adr_ptr_ok = "T" ) ;
	    %End
	Else
	    %^ This must be an AUX hit, need to set the Pointer
	   %beg
	    Ent_debit_set (.Obk_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.obk_adr_ptr_ok = "T" ) ;
	   %end
	END-IF.

*
* If there's a zip code, call subroutine to put it into address
*
	IF (Zip of Relget_adr_set NOT = SPACES ) THEN
	    Call "ZIPSUB" USING
		BY REFERENCE Obk_name4 of Ent_debit_set
		BY REFERENCE Obk_name4_length of Ent_debit_set_lengths
		BY REFERENCE Obk_name3 of Ent_debit_set
		BY REFERENCE Obk_name3_length of Ent_debit_set_lengths
		BY REFERENCE Zip OF Relget_adr_set,
		BY REFERENCE Line_flg_ws
	    If (Line_flg_ws = "3" ) 
		%Beg  Ent_debit_set.Obk_name3 CHANGE ;  %End
	    ELSE
		%Beg  Ent_debit_set.Obk_name4 CHANGE ;  %End
	    END-IF
	END-IF.

B220_OBK_MAP_CLEANUP.

	If obk_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "OBK"
	       by reference obk_idtype of Ent_debit_set
	       by reference obk_id of Ent_debit_set
	       by reference obk_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference obk_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.obk_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

        %Beg  BREAK: Relget_adr_set ;  %End.

	Set Ins_LOOKUP in Dbt_screen_state to True.

B220_OBK_MAP_END.
	EXIT.

B230_INS_LOOKUP.
%^
	If (Ins of Ent_debit_set = Ins of Change_debit_set  )	Or %^ No change
	   (Ins of Ent_debit_set = Spaces and
	    Ins of Change_debit_set = Spaces)
	Then
		Set SBK_LOOKUP in Dbt_screen_state to True
		Go to B230_Ins_Lookup_End
	End-if.
%^
*  Paragraph to do account lookup on the INS; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.

	If (Ins_adr_ptr_ok of flgs3 of ent_debit_set NOT = Spaces)
	then	%^ hose out prior lookup
	    %beg
	    Ent_debit_set (.Ins_rel_id     =  <0>,
			   .Ins_adr_set_ptr DELETE,
			   .flgs3.Ins_adr_ptr_ok = NULL ) ;
	    %end
	end_if.

	Move ZERO to Relget_msgcode.
* put in N or @ id if left blank by operator
	IF (Ins_idtype of Ins of Ent_debit_set = "N" OR "@")
	   AND (Ins_id of Ins of Ent_debit_set = SPACES)
	THEN
	    If Ins_name1 of Ent_debit_set NOT = SPACES
		MOVE SPACES TO Dsid_20charid_ws
		If Ins_idtype OF Ins OF Ent_debit_set = "N"
		    Call "AUTONAM" USING
			By reference Ins_name1 OF Ent_debit_set
			By reference Ins_name1_length of Ent_debit_set_lengths
			By reference Dsid_20charid_ws
			By reference Dsid_length
		ELSE
		    Call "CREATE_AUX_MNAM" USING 
			By reference Dsid_20charid_ws
			By reference Ins_name1_length of Ent_debit_set_lengths
			By reference Ins_name1 OF Ent_debit_set
			By reference Ins_name2_length OF Ent_debit_set_lengths
			By reference Ins_name2 OF Ent_debit_set
			By reference Ins_name3_length OF Ent_debit_set_lengths
			By reference Ins_name3 OF Ent_debit_set
			By reference Ins_name4_length OF Ent_debit_set_lengths
			By reference Ins_name4 OF Ent_debit_set
		END-IF
		MOVE %SIZ(Dsid_20charid_ws) TO Dsid_20charid_ws_length
		%Beg
		Dsid_compose ^OUT(Ent_debit_set.Ins.Ins_id), 
				Dsid_20charid_ws, /;
		%End
	    ELSE
*		error, name missing
%^		%Beg	Ftrscr.debit.Ins_name1.msg = "FTRSCR$_NAMEMISSING";%End
		%beg
			field_ws = "DEBIT.Ins_name1";
			Mnemonic_ws = "FTRSCR$_NAMEMISSING" ;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Add 1 to Dsid_bogus_parties
		Add 1 to Error_memo_count_ls
		Set SBK_LOOKUP in Dbt_screen_state to True
		GO TO B230_Ins_Lookup_End
	    END-IF
	END-IF.

* no lookup if no id type
	If Ins_idtype of Ins of Ent_debit_set = SPACE
	Then
 	    Set SBK_LOOKUP in Dbt_screen_state to True
	    GO TO B230_Ins_Lookup_End
	END-IF.
	%Beg
	Dsid_parse ^IN(Ent_debit_set.Ins.Ins_id ) ^SPACE, Dsid_id_ws, 
		^SPACE, / ;
	%End.

	Set INS in Relget_title_flag to TRUE.
* Force the SWF lookup into the AUX database ONLY.
	If Ins_idtype of Ins of Ent_debit_set = "S"
	    Move "$" to Dsid_temp_idtype
	ELSE
	    Move Ins_idtype of Ins of Ent_debit_set To Dsid_temp_idtype
	END-IF.

	%Beg  Dsid_next_function = Menu_next_function;  %End
	%Beg  Menu_arg_cmd_vstr = "";  %End.
	Move SPACE to Dsid_lkup_pend_del.
	Call "REL_GET" USING 
	    by reference Dsid_temp_idtype
	    By reference Dsid_id_ws
	    By reference Dsid_id_ws_length
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	%^ Temp check, need a better method
	If Relget_msgcode  = "vmsg$_ambig_lookup"
	Then
	If Success_is in Relget_ambiguous_wf 
	Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Ins_Index_Conn 	in Dbt_screen_state  to True
		Go to B230_INS_LOOKUP_END
	end-if.

	%^ NEED a better exit stategy
	%^
	If Relget_reject_flag = "M"
	    Go to B230_Ins_lookup_end
	End-if.

	If dsid_next_function  not = Menu_next_function Then
		Go to B230_Ins_lookup_end
	end-if.
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
		Set Menu_is in Dbt_screen_state to True
	    Go to B230_Ins_lookup_end
	End-if.

	Set Ins_map in Dbt_screen_state to True.

B230_INS_LOOKUP_END.
	EXIT.

B230_INS_CONN.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.
%^
	Set INS_Map in Dbt_screen_state to True.
%^
B230_INS_CONN_END.
	EXIT.


B230_INS_MAP.

%^	Assume that we will continue
	Set SBK_Lookup in Dbt_screen_state to True.	

        If (Success_is in Dsid_ret_stat  )
	   OR (Relget_msgcode = Vmsg_dat_notonfile_wc )
        THEN
* We actually did get a REL or AUX db hit.
* Move RELGET's returned id into a vstr for checking.
	    %Beg
	    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
	    %End
	    Move "INS" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		GO TO B230_INS_MAP_CLEANUP
	    end-if
	ELSE
	    %Beg  Dsid_return_key = NULL ;  %End
	END-IF.

* If the IDTYPE or ID was changed by RELGET, move in the returned key
        If (Dsid_return_key NOT = spaces and Dsid_return_key_length NOT = 0)
            If (Relget_return_idtype NOT = Ins_idtype of Ins of Ent_debit_set)
            or (Dsid_return_key_length NOT = Dsid_id_ws_length)
            or (Dsid_return_key(1:Dsid_id_ws_length) NOT = Dsid_id_ws(1:Dsid_id_ws_length))
            THEN
* Pick up any changes that Relget made to the idtype or id.
		%Beg
		Ent_debit_set.Ins(
		    .Ins_idtype = Relget_return_idtype,
		    .Ins_id = Dsid_return_key);
		%End
           END-IF
	END-IF.

	If (Failure_is in Dsid_ret_stat   )
%^	    Move ZERO to Dsid_found_it
	    Set Failure_is in Dsid_found_it to True
	    If (Ins_rel_id of Ent_debit_set NOT = 0 )
* Pre-owned INS, reset by user.  Flush it.
		%Beg
		Ent_debit_set (.Ins_rel_id     =  <0>,
			       .Ins_adr_set_ptr DELETE,
			       .flgs3.Ins_adr_ptr_ok = NULL ) ;
%^		Dsid_found_it = <1> ;
		%End
		Set Success_is in Dsid_found_it to TRUE
	    END-IF
	    If (Relget_msgcode NOT = Vmsg_dat_notonfile_wc )
%^		If (Dsid_found_it NOT = 0 )
		If (Success_is in Dsid_found_it)
* Current INS address was from a previous lookup
		    %Beg
%^ Clear out any lingering information.
		    Ent_debit_set (.Ins_name1      = NULL,
			           .Ins_name2      = NULL,
			           .Ins_name3      = NULL,
			           .Ins_name4      = NULL,
			           .Ins_adr_bnk_id = NULL,
				   .Ins_res_country = NULL ) ;
		    %End
		END-IF
	        If (Ins_idtype of Ins of Ent_debit_set = "S")
	           AND (Swift_adr_nof_warning of Menu_cfg = "T")
	        THEN
%^18-May-89 For SWIFT ID's, set fatal error if INTRTL flag is set
%^		    %Beg Ftrscr.debit.Ins.Ins_id.msg = "VMSG$_LOOKUPFAIL"; %End
		    %beg
			field_ws = "DEBIT.INS.INS_ID";
			Mnemonic_ws = "VMSG$_LOOKUPFAIL" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		    Add 1 to Dsid_bogus_parties
		    Add 1 to Error_memo_count_ls
 		    Set Success_is in dsid_lower_party_erred to True
		    %^Set Failure_is in Debitside_screen_status_ls to true
	        END-IF
	        GO TO B230_INS_MAP_CLEANUP
	    END-IF
	END-IF.

%^ We now have at least copy-able INS info.
	%Beg
	Ent_debit_set (.Ins_name1 = Relget_adr_set.adr_name,
		       .Ins_name2 = Relget_adr_set.adr1,
		       .Ins_name3 = Relget_adr_set.adr2,
		       .Ins_name4 = Relget_adr_set.adr3,
		       .Ins_adr_bnk_id = Relget_adr_set.bnk_id) ;
	%End.

	If (Rel_id of Relget_adr_set NOT = ZERO )
	    %Beg
	    Ent_debit_set (.Ins_rel_id = Relget_adr_set.rel_id,
			   .Ins_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.Ins_adr_ptr_ok = "T" ) ;
	    %End
	Else
	    %^ This must be an AUX hit, need to set the Pointer
	   %beg
	    Ent_debit_set (.Ins_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.Ins_adr_ptr_ok = "T" ) ;
	   %end
	END-IF.

*
* If there's a zip code, call subroutine to put it into address
*
	IF (Zip of Relget_adr_set NOT = SPACES ) THEN
	    Call "ZIPSUB" USING
		BY REFERENCE Ins_name4 of Ent_debit_set
		BY REFERENCE Ins_name4_length of Ent_debit_set_lengths
		BY REFERENCE Ins_name3 of Ent_debit_set
		BY REFERENCE Ins_name3_length of Ent_debit_set_lengths
		BY REFERENCE Zip OF Relget_adr_set,
		BY REFERENCE Line_flg_ws
	    If (Line_flg_ws = "3" ) 
		%Beg  Ent_debit_set.Ins_name3 CHANGE ;  %End
	    ELSE
		%Beg  Ent_debit_set.Ins_name4 CHANGE ;  %End
	    END-IF
	END-IF.

B230_INS_MAP_CLEANUP.

	If Ins_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "INS"
	       by reference Ins_idtype of Ent_debit_set
	       by reference Ins_id of Ent_debit_set
	       by reference Ins_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference Ins_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.Ins_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

        %Beg  BREAK: Relget_adr_set ;  %End.

	Set Sbk_LOOKUP in Dbt_screen_state to True.

B230_INS_MAP_END.
	EXIT.



B240_SBK_LOOKUP.
*  Paragraph to do account lookup on the SBK; fills out name and address
*    fields if we found it.  This is a pure address lookup -- no accounting.
*    If the lookup fails, that's okay -- the name and address may already
*    be there.
	If (Failure_is in Sbk_changed_ls)	%^ No change
	Then
		Set DBT_LOOKUP1 in Dbt_screen_state to True
		Go to B240_SBK_Lookup_End
	End-if.


	If (sbk_adr_ptr_ok of flgs3 of ent_debit_set NOT = Spaces)
	then	%^ hose out prior lookup
	    %beg
	    Ent_debit_set (.sbk_rel_id     =  <0>,
			   .sbk_adr_set_ptr DELETE,
			   .flgs3.sbk_adr_ptr_ok = NULL ) ;
	    %end
	end_if.

	Move ZERO to Relget_msgcode.
* put in N or @ id if left blank by operator
	IF (Sbk_idtype of Sbk of Ent_debit_set = "N" OR "@")
	   AND (Sbk_id of Sbk of Ent_debit_set = SPACES)
	THEN
	    If Sbk_name1 of Ent_debit_set NOT = SPACES
		MOVE SPACES TO Dsid_20charid_ws
		If Sbk_idtype OF Sbk OF Ent_debit_set = "N"
		    Call "AUTONAM" USING
			By reference Sbk_name1 OF Ent_debit_set
			By reference Sbk_name1_length OF Ent_debit_set_lengths
			By reference Dsid_20charid_ws
			By reference Dsid_length
		ELSE
		    Call "CREATE_AUX_MNAM" USING 
			By reference Dsid_20charid_ws
			By reference Sbk_name1_length of Ent_debit_set_lengths
			By reference Sbk_name1 OF Ent_debit_set
			By reference Sbk_name2_length OF Ent_debit_set_lengths
			By reference Sbk_name2 OF Ent_debit_set
			By reference Sbk_name3_length OF Ent_debit_set_lengths
			By reference Sbk_name3 OF Ent_debit_set
			By reference Sbk_name4_length OF Ent_debit_set_lengths
			By reference Sbk_name4 OF Ent_debit_set
		END-IF
		MOVE %SIZ(Dsid_20charid_ws) TO Dsid_20charid_ws_length
		%Beg
		Dsid_compose ^OUT(Ent_debit_set.Sbk.Sbk_id), 
				Dsid_20charid_ws, /;
		%End
	    ELSE
*		error, name missing
%^		%Beg	Ftrscr.debit.sbk_name1.msg = "FTRSCR$_NAMEMISSING"; %End
		%beg
			field_ws = "DEBIT.Sbk_name1";
			Mnemonic_ws = "FTRSCR$_NAMEMISSING" ;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Add 1 to Dsid_bogus_parties
		Add 1 to Error_memo_count_ls
		%^	Set  Error_is in Dbt_screen_state to True
	        Set DBT_LOOKUP1 in Dbt_screen_state to True
		GO TO B240_SBK_LOOKUP_END
	    END-IF
	END-IF.

* no lookup if no id type
* removed this code so we will fall through and initialize the dbt_rel_id if
* there is no idtype, or id.
	%Beg
	Dsid_parse ^IN(Ent_debit_set.Sbk.Sbk_id ) ^SPACE, Dsid_id_ws, 
		^SPACE, / ;
	%End.

	Set SBK in Relget_title_flag to TRUE.
	%Beg  Dsid_next_function = Menu_next_function;  %End.
	%Beg  Menu_arg_cmd_vstr = "";  %End.
	Move SPACE to Dsid_lkup_pend_del.
	Call "REL_GET" USING 
	    By reference Sbk_idtype of Sbk of Ent_debit_set
	    By reference Dsid_id_ws
	    By reference Dsid_id_ws_length
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	If Success_is in Relget_ambiguous_wf
	Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Sbk_Index_Conn1 	in Dbt_screen_state  to True
		Go to B240_SBK_LOOKUP
	End-if.

	If Relget_reject_flag = "M"
	    Go to B240_SBK_LOOKUP_END
	End-if.

	If dsid_next_function not = Menu_next_function Then
		GO TO B240_SBK_LOOKUP_END
	END-IF.	
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    GO TO B240_SBK_LOOKUP_END
	End-if.
   
	Set Sbk_lookup2 in Dbt_screen_state to True.

B240_SBK_LOOKUP_END.
	EXIT.

B240_SBK_INDEX_CONN1.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	Set Sbk_lookup2 in Dbt_screen_state to True.

B240_SBK_INDEX_CONN1_END.
	EXIT.


B240_SBK_LOOKUP2.
%^
%^ SBK has a secondary lookup for AUX
%^
	If (Success_is in Dsid_ret_stat )
	   OR (Relget_msgcode = Vmsg_dat_notonfile_wc )
        THEN
* We actually did get a REL or AUX db hit.
* Move RELGET's returned id into a vstr for checking.
	    %Beg
	    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
	    %End
	    Move "SBK" to Bcc_lkup_party
	    Perform D100_BCC_LKUP_BIC thru D100_BCC_LKUP_BIC_END
	    If Failure_is in Bcc_lkup_status
		Perform B240_SBK_RES_COUNTRY Thru
			B240_SBK_RES_COUNTRY_END
		Set Complete_is in Dbt_screen_state to True
		Go to B240_SBK_LOOKUP2_END
	    end-if
   	ELSE
	    %Beg  Dsid_return_key = NULL ;  %End
	END-IF.

	Set Failure_is in Swf_aux_lookup_ws to True.

* Check for lengthening an 8-char SWIFT into the only onfile branch.
	If (Sbk_idtype of Sbk of Ent_debit_set = "S" )
	    AND (Dsid_id_ws_length = 8 )
	    AND (Dsid_return_key_length > Dsid_id_ws_length )
	THEN
	    Set Success_is in Swf_aux_lookup_ws to True
* Please try again on the AUX database.
            %Beg
            Dsid_sav_idtype = Relget_return_idtype;
            Dsid_sav_ret_stat = Dsid_ret_stat;
            Dsid_sav_relget_msgcode = Relget_msgcode;
	    BREAK: Dsid_sav_adr_set;
            Relget_adr_set EQUATE: Dsid_sav_adr_set(NOMOD);
            BREAK: Relget_adr_set ;
            %End
	    Move SPACE to Dsid_lkup_pend_del
%^
%^
%^ This should be "$" now "s", so search on
%^ Aux_db_set.Bic_tape_swf_index CONN: Relget_index;
%^	
	    Call "REL_GET" USING 
	        By content "s"
	        By reference Dsid_id_ws
	        By reference Dsid_id_ws_length
	        by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret_stat
	   If Success_is in Relget_ambiguous_wf
	   Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Sbk_Index_Conn2 	in Dbt_screen_state  to True
		Go to B240_SBK_LOOKUP2_END
	   End-if
	End-if.

	Set Sbk_map in Dbt_screen_state to True.

B240_SBK_LOOKUP2_END.
	Exit.

B240_SBK_INDEX_CONN2.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	Set Sbk_map in Dbt_screen_state to True.

B240_SBK_INDEX_CONN2_END.
	EXIT.

B240_SBK_RES_COUNTRY.
%^ kludgy call to allow BCC code to exit.
	If sbk_res_country of ent_debit_set = spaces
   	 	Initialize Dsid_risk_country_ws, Dsid_country_code_ws
    	    If rel_id of relget_adr_set not = 0
	  	%Ace_is relget_adr_set connected;
		If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "SBK"
	       by reference sbk_idtype of Ent_debit_set
	       by reference sbk_id of Ent_debit_set
	       by reference sbk_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference sbk_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.sbk_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

	%Beg  BREAK: Relget_adr_set ;  %End.
B240_SBK_RES_COUNTRY_END.
	EXIT.


B240_SBK_MAP.
* Check for lengthening an 8-char SWIFT into the only onfile branch.

	If Success_is in Swf_aux_lookup_ws
	Then
	    If (Relget_msgcode = Vmsg_dat_notonfile_wc )
		AND (Relget_return_key NOT = SPACES )
	    THEN
	        %Beg
	        Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
                BREAK: Dsid_sav_adr_set;
	        %End
            ELSE
                %Beg
                Relget_return_idtype = Dsid_sav_idtype;
                Dsid_ret_stat = Dsid_sav_ret_stat;
                Relget_msgcode = Dsid_sav_relget_msgcode;
                BREAK: Relget_adr_set ;
                Dsid_sav_adr_set EQUATE: Relget_adr_set(NOMOD);
                BREAK: Dsid_sav_adr_set ;
                %End
	    END-IF
	END-IF.

* If the IDTYPE or ID were changed by RELGET, move in the returned key
        If (Dsid_return_key NOT = spaces and Dsid_return_key_length NOT = 0)
            If (Relget_return_idtype NOT = Sbk_idtype of Sbk of Ent_debit_set)
            or (Dsid_return_key_length NOT = Dsid_id_ws_length)
            or (Dsid_return_key(1:Dsid_id_ws_length) NOT = Dsid_id_ws(1:Dsid_id_ws_length))
            THEN
* Pick up any changes that Relget made to the idtype or id.
		%Beg
		Ent_debit_set.Sbk(
		    .Sbk_idtype = Relget_return_idtype,
		    .Sbk_id = Dsid_return_key);
		%End
            END-IF
	END-IF.

	If (Failure_is in Dsid_ret_stat   )
%^	    Move ZERO to Dsid_found_it
	    Set Failure_is in Dsid_found_it to True
	    If (Sbk_rel_id of Ent_debit_set NOT = 0 )
* Pre-owned SBK, reset by user.  Flush it.
		%Beg
		Ent_debit_set (.sbk_rel_id     =  <0>,
			       .sbk_adr_set_ptr DELETE,
			       .flgs3.sbk_adr_ptr_ok = Null) ;
%^		Dsid_found_it = <1> ;
		%End
		Set Success_is in Dsid_found_it to TRUE
		%^ Flush any delivery standing instruction sequence from the
		%^ previous SBK party.

	    END-IF
	    If (Relget_msgcode NOT = Vmsg_dat_notonfile_wc )
%^		If (Dsid_found_it NOT = 0 )
		If (Success_is in Dsid_found_it)
* Current SBK address was from a previous lookup
		    %Beg
%^ Clear out any lingering information.
		    Ent_debit_set (.sbk_name1      = NULL,
			           .sbk_name2      = NULL,
			           .sbk_name3      = NULL,
			           .sbk_name4      = NULL,
			           .sbk_adr_bnk_id = NULL,
				   .sbk_res_country = NULL ) ;
		    Ent_debit_set.sbk_bei_flag = Null;
		    %End
		END-IF
	        If (Sbk_idtype of Sbk of Ent_debit_set = "S")
	           AND (Swift_adr_nof_warning of Menu_cfg = "T")
	        THEN
%^18-May-89 For SWIFT ID's, set fatal error if INTRTL flag is set
%^		    %Beg Ftrscr.debit.sbk.sbk_id.msg = "VMSG$_LOOKUPFAIL";%End
		    %beg
			field_ws = "DEBIT.SBK.SBK_ID";
			Mnemonic_ws = "VMSG$_LOOKUPFAIL" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		    Add 1 to Dsid_bogus_parties
		    Add 1 to Error_memo_count_ls
 		    Set Success_is in dsid_lower_party_erred to True
		    %^Set Failure_is in Debitside_screen_status_ls to true
	        END-IF
	        GO TO B240_SBK_MAP_CLEANUP
	    END-IF
	END-IF.
%^ We now have at least copy-able SBK info.
	%Beg
	Ent_debit_set (.Sbk_name1 = Relget_adr_set.adr_name,
		       .Sbk_name2 = Relget_adr_set.adr1,
		       .Sbk_name3 = Relget_adr_set.adr2,
		       .Sbk_name4 = Relget_adr_set.adr3,
		       .Sbk_adr_bnk_id = Relget_adr_set.bnk_id) ;
	%End.

	If (Rel_id of Relget_adr_set NOT = ZERO )
	    %Beg
	    Ent_debit_set (.Sbk_rel_id = Relget_adr_set.rel_id,
			   .Sbk_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.sbk_adr_ptr_ok = "T" ) ;
	    %End

  
	Else
	    %^ This must be an AUX hit, need to set the Pointer
	   %beg
	    Ent_debit_set (.Sbk_adr_set_ptr POINT: Relget_adr_set,
			   .flgs3.sbk_adr_ptr_ok = "T" ) ;
	   %end
	END-IF.

	If sbk_idtype of sbk of Ent_debit_set  = "S" Then
		If swf_subtype of relget_adr_set  = "BEID" or "CORP" or "MCCO" or "SMDP" or "TESP" or "TRCO" Then
			%beg  ent_debit_set.sbk_bei_flag = "Y"; %end
		else
			%beg  ent_debit_set.sbk_bei_flag = Null; %end
		end-if
	end-if.
*
* If there's a zip code, call subroutine to put it into address
*
	IF (Zip of Relget_adr_set NOT = SPACES ) THEN
	    Call "ZIPSUB" USING
		BY REFERENCE Sbk_name4 of Ent_debit_set
		BY REFERENCE Sbk_name4_length of Ent_debit_set_lengths
		BY REFERENCE Sbk_name3 of Ent_debit_set
		BY REFERENCE Sbk_name3_length of Ent_debit_set_lengths
		BY REFERENCE Zip OF Relget_adr_set,
		BY REFERENCE Line_flg_ws
	    If (Line_flg_ws = "3" ) 
		%Beg  Ent_debit_set.Sbk_name3 CHANGE ;  %End
	    ELSE
		%Beg  Ent_debit_set.Sbk_name4 CHANGE ;  %End
	    END-IF
	END-IF.
   
	%ACE_IS relget_adr_set Connected giving Dsid_ret_stat;.

	%Beg
	BREAK: Prulepty_rule_seq(NOMOD);
	BREAK: Prulepty_party_adr_set(NOMOD);
	%End
	If Success_is in Dsid_ret_stat Then
		%beg Prulepty_source rel_is;
		     Prulepty_party_adr_ok Success_is;
	     	     Relget_adr_set.adr_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Relget_adr_set EQUATE: Prulepty_party_adr_set(NOMOD);
		%end
		If (Rel_id of Relget_adr_set = 0 )
		    %Beg  Prulepty_source aux_is;  %End
		end-if
	Else
		%beg Prulepty_source host_is;
		     Ent_debit_set.Sbk_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Prulepty_party_adr_ok Failure_is;
		%end
		Initialize Prulepty_party_adr_set
	end-if.

	%beg
	    Dsid_id_bank_ws = Ent_debit_set.Sbk_adr_bnk_id;
	    Prulepty_bank_prof_rec = NULL;
	%end
	If (Dsid_id_bank_ws = SPACES)
	    %Beg  Dsid_id_bank_ws = Ent_ftr_set.Loc_info.Bank;  %End
	end-if

	If (Dsid_id_bank_ws = Bnk_id of Menu_bnk_union)
		%beg
		     Prulepty_bank_prof_rec =
					     Menu_bnk_union.Bnk_profile_id_rec;
		%end
	else
		%Beg
		   BREAK: Dsid_bnk_union ;
		   SEARCH: Bnk_index 
			   (Key = Dsid_id_bank_ws );
		%end
 		If (Success_is in Bnk_index_status)
			%Beg Bnk_index CONN: Dsid_bnk_union(NOMOD); %end
		end-if
		%beg
  		   Prulepty_bank_prof_rec = Dsid_bnk_union.Bnk_profile_id_rec;
		%End
	end-if.
 
	Call "PRULE_CHANGE_SENDER" Returning Dsid_ret_stat.

	%beg dsid_upd_level sbk_is; %end
	Call "PRULE_UPDATE_PARTY" Using
		By Reference  dsid_upd_level
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.

B240_SBK_MAP_CLEANUP.

	If sbk_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "SBK"
	       by reference sbk_idtype of Ent_debit_set
	       by reference sbk_id of Ent_debit_set
	       by reference sbk_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference sbk_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.sbk_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

	%Beg  BREAK: Relget_adr_set ;  %End.

	Set DBT_LOOKUP1 in Dbt_screen_state to True.


B240_SBK_MAP_END.
	EXIT.


B260_DBT_LOOKUP1.
	If (Debit_changed_ls = 0)
	Then
		Set Complete_is in Dbt_screen_state to True
		Go to B260_DBT_LOOKUP1_END
	End-if.

* Paragraph to do actual lookup of debit party.
	%Beg  BREAK: Ent_d_adr_set ;  %End.
	%^ clear res_country, for benefit of Set_debit_address
	%^ obly clear if party changed, but country did not
	If ( dbt_res_country of Ent_debit_set = dbt_res_country of Change_debit_set ) And
	   ( dbt_typ of ent_debit_set NOT = dbt_typ of Change_debit_set )
	Then
		%beg ent_debit_set.dbt_res_country = Null; %end
	end-if.
	If (dbt_account of Ent_debit_set NOT = SPACES )
            %Beg
            Ent_debit_set (
                        .dbt_account            = NULL,
                        .dbt_acc_prod_codes     = NULL,
                        .dbt_acc_class          = NULL ) ;
	    %End
	END-IF.
	Move ZERO to Relget_msgcode.
* put in N or @ id if left blank by operator
	IF (Dbt_idtype of Dbt_typ of Ent_debit_set = "N" OR "@")
	   AND (Dbt_id of Dbt_typ of Ent_debit_set = SPACES)
	THEN
	    If Dbt_name1 of Ent_debit_set NOT = SPACES
		MOVE SPACES TO Dsid_20charid_ws
		If Dbt_idtype OF Dbt_typ OF Ent_debit_set = "N"
		    Call "AUTONAM" USING
			By reference Dbt_name1 OF Ent_debit_set
			By reference Dbt_name1_length OF Ent_debit_set_lengths
			By reference Dsid_20charid_ws
			By reference Dsid_length
		ELSE
		    Call "CREATE_AUX_MNAM" USING 
			By reference Dsid_20charid_ws
			By reference Dbt_name1_length of Ent_debit_set_lengths
			By reference Dbt_name1 OF Ent_debit_set
			By reference Dbt_name2_length OF Ent_debit_set_lengths
			By reference Dbt_name2 OF Ent_debit_set
			By reference Dbt_name3_length OF Ent_debit_set_lengths
			By reference Dbt_name3 OF Ent_debit_set
			By reference Dbt_name4_length OF Ent_debit_set_lengths
			By reference Dbt_name4 OF Ent_debit_set
		END-IF
		MOVE %SIZ(Dsid_20charid_ws) TO Dsid_20charid_ws_length
		%Beg
		Dsid_compose ^OUT(Ent_debit_set.Dbt_typ.Dbt_id), 
				Dsid_20charid_ws, /;
		%End
	    ELSE
*		error, name missing
%^		%Beg Ftrscr.debit.dbt_name1.msg = "FTRSCR$_NAMEMISSING";%End
		%beg
			field_ws = "DEBIT.DBT_NAME1";
			Mnemonic_ws = "FTRSCR$_NAMEMISSING" ;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Add 1 to Error_memo_count_ls
		Set Error_is in dbt_screen_state to True
		Set Failure_is in Debitside_screen_status_ls to true
		GO TO B260_DBT_LOOKUP1_END
	    END-IF
	END-IF.

* no lookup if no id type
	If Dbt_idtype of Dbt_typ of Ent_debit_set = SPACE
%^ Nothing to look up.
	    %Beg  Dsid_id_ws = Ent_debit_set.Dbt_typ.Dbt_id ;  %End
	    If (Dsid_id_ws_length NOT = 0 ) Or
	       (dbt_ovr of dbt_typ of Ent_debit_set NOT = "*") Then
		%Beg
		Ent_debit_set.Dbt_typ.dbt_ovr = "*" ;
		%End
	    END-IF
	    Set Complete_is in dbt_screen_state to True
	    GO TO B260_DBT_LOOKUP1_END	%^ WAS _EXIT
	END-IF.
	%Beg
	Dsid_clip_compose ^OUT(Dsid_party_id)
		Ent_debit_set.Dbt_typ.Dbt_id, / ;
	%End.

%^ if this is a return of a drawdown request, Aba is a valid debit party, allow
%^ REL_GET to do the AUX lookup by telling it to lookup as if
%^ a sending bank ...KB 49721
	If type_code of ent_ftr_set(3:2) = "33" then
		Set SBK in Relget_title_flag to TRUE
	Else
		Set DBT in Relget_title_flag to TRUE
	End-if.

	%Beg
%^	Dsid_ftrscr_wf = Ftrscr State.Conn;
%^	Dsid_disp_only_wf = Ftrscr.Debit.Dbt_typ.Dbt_idtype.Disp_only;
		Dsid_next_function = Menu_next_function;
		Menu_arg_cmd_vstr = "";
	%End.

%^ KB Need to check how the Connect would work, I'd ASSUME that it is Connected
	Set Success_is in dsid_ftrscr_wf to True.

	%beg	Field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE"; %end
	call "FTRSCR_GET_DISP_ONLY" using
		by reference field_ws
		returning dsid_disp_only_wf
	%beg
		field_ws = "MSG1";
		Mnemonic_ws = "";
        %End
	call "FTRSCR_WRITE_ERROR" using
		by reference field_ws
		by reference Mnemonic_ws
	%beg
		field_ws = "MSG2";
		Mnemonic_ws = "";
        %End
	call "FTRSCR_WRITE_ERROR" using
		by reference field_ws
		by reference Mnemonic_ws

%^	End-if.

	Move "F" to Dsid_lkup_pend_del.
	Call "REL_GET" USING 
	    By reference Dbt_idtype of Dbt_typ of Ent_debit_set
	    By reference Dsid_party_id
	    By reference Dsid_party_id_length
	    by reference Dsid_lkup_pend_del
	RETURNING Dsid_ret_stat.

	If Success_is in Relget_ambiguous_wf 
	Then
		Set Success_is  	in Resolve_Ambiguous to True
		Set Dbt_Index_Conn1 	in Dbt_screen_state  to True
		Go to B260_DBT_LOOKUP1_END
	End-if.

	If Relget_reject_flag = "M"
	    %Exit Program
	End-if.

	If Dsid_next_function not = Menu_next_function Then
		%Exit Program
		%^Go to B260_debit_party_end
	End-if.
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    %Exit Program
	End-if.

	Set Dbt_Lookup2 in Dbt_screen_state to True.

B260_DBT_LOOKUP1_END.
	EXIT.

B260_DBT_INDEX_CONN1.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	Set Dbt_lookup2 in Dbt_screen_state to True.

B260_DBT_INDEX_CONN1_END.
	EXIT.

B260_DBT_LOOKUP2.

	If Dsid_lkup_pend_del = "T"
         %^  %Beg  Ftrscr.Debit.dbt_typ.dbt_id.Msg = "VMSG$_ADR_PEND_DEL";  %End
	    %beg
			field_ws = "DEBIT.DBT_TYP.DBT_ID";
			Mnemonic_ws = "VMSG$_ADR_PEND_DEL" ;
            %End
      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
            Add 1 to Error_memo_count_ls
	    Set Success_is in Dsid_debit_pend_del to TRUE
	    Set Failure_is in Debitside_screen_status_ls to TRUE
        End-if
        If (PD_enqueue_to_server of Menu_cfg = "T")
	   AND (Relget_msgcode = Vmsg_lookup_fail_wc)
	   AND (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = "*")
	THEN
%^ We have an ambiguous address - write memo and let calling routine
%^  route message.
            %Beg
            Dsid_compose ^OUT(Dsid_info_memo)
                    "Ambiguous Address for Account: ",
		     Ent_Debit_Set.Dbt_Typ.Dbt_Idtype, "/",
                     Ent_Debit_Set.Dbt_Typ.Dbt_Id, / ;
            %End
            Add 1 to Dsid_bogus_parties
            Perform X920_INFO_MEMO through X920_INFO_MEMO_END
        End-if.
        If (Success_is in Dsid_ret_stat  )
	   OR (Relget_msgcode = Vmsg_dat_notonfile_wc )
        THEN
* We actually did get a REL or AUX db hit.
* Move RELGET's returned id into a vstr for checking.
	    %Beg
	    Dsid_clip_compose ^OUT(Dsid_return_key)
		Relget_return_key, / ;
	    %End
	ELSE
	    %Beg  Dsid_return_key = NULL ;  %End
	END-IF.

	Set Failure_is in Swf_aux_lookup_ws to True.

* Check for lengthening an 8-char SWIFT into the only onfile branch.
	If (Dbt_idtype of Dbt_typ of Ent_debit_set = "S" )
	    AND (Dsid_id_ws_length = 8 )
	    AND (Dsid_return_key_length > Dsid_id_ws_length )
	    AND (Dsid_lkup_pend_del NOT = "T" )
	THEN

	    Set Success_is in Swf_aux_lookup_ws to True
	    
* Please try again on the AUX database.
            %Beg
            Dsid_sav_idtype = relget_return_idtype;
            Dsid_sav_ret_stat = Dsid_ret_stat;
            Dsid_sav_relget_msgcode = Relget_msgcode;
	    BREAK: Dsid_sav_adr_set;
            Relget_adr_set EQUATE: Dsid_sav_adr_set(NOMOD);
            BREAK: Relget_adr_set ;
	    dsid_next_function = Menu_next_function;
	    Menu_arg_cmd_vstr = "";
	    %End
	    Move "F" to Dsid_lkup_pend_del
	    Call "REL_GET" USING 
	        By content "s"
	        By reference Dsid_id_ws
	        By reference Dsid_id_ws_length
	        by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret_stat
	If Success_is in Relget_ambiguous_wf 
	Then
		Set Success_is in Resolve_ambiguous to True
		Set Sbk_Index_Conn2 	in Dbt_screen_state  to True
		Go to B260_DBT_LOOKUP2_END
	   End-if
	End-if.

	Set Dbt_map in Dbt_screen_state to True.

B260_DBT_LOOKUP2_END.
	Exit.

B260_DBT_INDEX_CONN2.
%^
%^ Re-enter here after return from ambiguous screen and 
%^ Connect the indices required.  
%^
	Call "RELGET_CONNECT" USING 
	    by reference Dsid_lkup_pend_del
	  RETURNING Dsid_ret_stat.

	Set Dbt_map in Dbt_screen_state to True.

B260_DBT_INDEX_CONN2_END.
	EXIT.

B260_DBT_MAP.
* Check for lengthening an 8-char SWIFT into the only onfile branch.
	If Success_is in Swf_aux_lookup_ws Then
	    If (Relget_msgcode = Vmsg_dat_notonfile_wc )
		AND (Relget_return_key NOT = SPACES )
		AND (Dsid_lkup_pend_del NOT = "T" )
	    THEN
	        %Beg
	        Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
                BREAK: Dsid_sav_adr_set;
	        %End
            ELSE
                %Beg
                Relget_return_idtype = Dsid_sav_idtype;
                Dsid_ret_stat = Dsid_sav_ret_stat;
                Relget_msgcode = Dsid_sav_relget_msgcode;
                BREAK: Relget_adr_set ;
                Dsid_sav_adr_set EQUATE: Relget_adr_set(NOMOD);
                BREAK: Dsid_sav_adr_set ;
                %End
	    END-IF
	END-IF.


* If the IDTYPE or ID were changed by RELGET, move in the returned key
        If (Dsid_return_key NOT = SPACES)
	   AND (Dsid_return_key_length NOT = 0)
	THEN
            If (Relget_return_idtype NOT =
					Dbt_idtype of Dbt_typ of Ent_debit_set)
               OR (Dsid_return_key(1:Dsid_party_id_length) NOT =
					      Dsid_party_id(1:Dsid_party_id_length) )
	       OR ((Bnk_id of Relget_adr_set NOT = Dbt_adr_bnk_id of Ent_debit_set) AND
		     (Dbt_adr_bnk_id of Ent_debit_set not = Spaces))
	       OR ((Dbt_adr_bnk_id of Ent_debit_set = Spaces) AND
		     (Bnk_id of Relget_adr_set NOT = Bnk_id of Menu_bnk_union))
            THEN
* Pick up any changes that Relget made to the idtype or id.
* >>>> only if debit party is not protected from update
		If Success_is in Dsid_ftrscr_wf and 
			Success_is in Dsid_disp_only_wf and
                        Idfunc of Menu_next_function = "RPR" and
                        Dbt_idtype of Dbt_typ of Ent_debit_set = "A"
                    %Beg
%^                    Ftrscr(
%^			.Msg1 = "Debit party can not be modified. Original data restored.",
%^			.Msg2 = "");
		    Ent_debit_set.Dbt_typ.Dbt_ovr = "?";
                    %End
		    %beg
			field_ws = "MSG1";
			Mnemonic_ws =  "Debit party can not be modified. Original data restored.";
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws


		    Add 1 to Error_memo_count_ls
		    Set Failure_is in Debitside_screen_status_ls to True
		    Go to B260_DBT_MAP_END
		else
	            Move Relget_return_idtype to
					Dbt_idtype of Dbt_typ of Ent_debit_set
	            Move SPACES to Dbt_id of Dbt_typ of Ent_debit_set
	            %Beg  Ent_debit_set.dbt_typ.dbt_id = Dsid_return_key ;  %End
		End-if
            END-IF 
	END-IF.

	If (Failure_is in Dsid_ret_stat )
	or (Relget_msgcode = Vmsg_dat_notonfile_wc )	%^ Aux Hit, still NOF
	%^	   OR (Dsid_ovr NOT = SPACE )		%^ Dsid_ovr has no meaning here.....
	THEN
	    If (Dbt_rel_id of Ent_debit_set NOT = 0 )
* Pre-owned debit party reset by user.  Flush it.
		%Beg
		Ent_debit_set (.dbt_name1      = NULL,
			       .dbt_name2      = NULL,
			       .dbt_name3      = NULL,
			       .dbt_name4      = NULL,
			       .dbt_res_country = NULL) ;
		BREAK: Ent_d_adr_set ;
		%End
                Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
	        Move "N" to Dbt_comm_charge_ws
	        Move "N" to Dbt_cbl_charge_ws
	    END-IF
	    If (Failure_is in Dsid_ret_stat)
	        If (Success_is in Dsid_debit_pend_del)
		    %Beg  Ent_debit_set.dbt_typ.dbt_ovr = "?";  %End
	        ELSE
		    %Beg  Ent_debit_set.dbt_typ.dbt_ovr = "*" ;  %End
	        END-IF
	        Move SPACES to Dbt_country_code_ws
	    END-IF

            Call "SET_NOF_DBT_BNK_ID" using
		by reference Nochange_bank_ls
	        by reference Dsid_loc_bank_change
	      RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		set Success_is in Msg_bank_changed_ls to true
	    END-IF
            If Success_is in Msg_bank_changed_ls and Success_is in Nochange_bank_ls
                %^ Write an error memo indicating the discrepancy.
%^                %Beg Ftrscr.debit.dbt_typ.dbt_idtype.msg = "FTRSCR$_NOXBNKDBTPRIV";                %End
		%beg
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                Add 1 to Error_memo_count_ls
                Set Failure_is in Debitside_screen_status_ls to true
            END-IF
            If Failure_is in Dsid_ret_stat   then
                %^ Write an error memo indicating the problem
%^              %Beg  Ftrscr.debit.dbt_typ.dbt_idtype.msg = "FTRSCR$_INVBANKSTR";%End
                %Beg
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_INVBANKSTR";
                 %End
	 	 call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                    Add 1 to Error_memo_count_ls
                Add 1 to Error_memo_count_ls
                Set Failure_is in Debitside_screen_status_ls to true
            END-IF

	    If (Relget_msgcode NOT = VMSG_DAT_NOTONFILE_WC )
		%Beg
		Ent_debit_set.dbt_adr_bnk_id = NULL ;
		%End
	    END-IF
	ELSE
	    If (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = SPACE )
		%Beg  Ent_debit_set.dbt_typ.dbt_ovr = " " ;  %End
	    END-IF
	END-IF.

	IF (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE )
	   OR ( (Dbt_ovr of Dbt_typ of Ent_debit_set = "*" )
	    	AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
* We actually did get a REL hit or an AUX db hit, so let's copy the address.
	THEN
%^ map the name and address from debit address party 
	    Perform C440_COPY_ADDRESS through C440_COPY_ADDRESS_END
	END-IF.

	%Beg  BREAK: Dsid_adr_set ;  %End.
	IF (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE )
	    %Beg  Relget_adr_set EQUATE: Dsid_adr_set(NOMOD) ;  %End
	END-IF.

B260_DBT_MAP_EXIT.

	Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END.

	Set Complete_is in Dbt_screen_state to True.

	If (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE) And
	   (Success_is in Debitside_screen_status_ls  ) 
	THEN
%^ Connect debit party address.
	        %Beg  
		Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;
                Ent_debit_set (
                        .Dbt_rel_id      =      Ent_d_adr_set.Rel_id,
                        .Dbt_adr_set_ptr POINT: Ent_d_adr_set,
			.flgs3.dbt_adr_ptr_ok = "T" );
		%End
                If (Bank of Loc_info of Ent_ftr_set = Bnk_id of Ent_d_adr_set )
                    If (Dbt_adr_bnk_id of Ent_debit_set NOT =
                                                      Bnk_id of Ent_d_adr_set )
                        %Beg
                        Ent_debit_set.Dbt_adr_bnk_id = Ent_d_adr_set.Bnk_id ;
                        %End
                    END-IF
                ELSE
                    If (Bank of Loc_info of Ent_ftr_set = SPACES )
                        %Beg
                        Ent_ftr_set.loc_info.bank = Ent_d_adr_set.Bnk_id ;
                        Ent_debit_set.Dbt_adr_bnk_id = Ent_d_adr_set.Bnk_id ;
                        %End
                    ELSE
%^ Already initialized to a different bank.
                        set Success_is in Msg_bank_changed_ls to true
                        If (Failure_is in Nochange_bank_ls )
                            %Beg
                            Ent_ftr_set.loc_info.bank = Ent_d_adr_set.Bnk_id ;
                            Ent_debit_set.Dbt_adr_bnk_id =
                                                         Ent_d_adr_set.Bnk_id ;
                            %End
                        END-IF
                    END-IF
                END-IF
	ELSE
* Yes, we have a NOF address.
* At least set the debit party bank to something reasonable.
                Call "SET_NOF_DBT_BNK_ID" using
		    by reference Nochange_bank_ls
	            by reference Dsid_loc_bank_change
	          RETURNING Dsid_ret_stat
		If (Dsid_loc_bank_change NOT = 0 )
		    set Success_is in Msg_bank_changed_ls to true
		END-IF
                If Success_is in Msg_bank_changed_ls
		   AND Success_is in Nochange_bank_ls
                    %^ Write an error memo indicating the discrepancy.
		    %beg
			field_ws = "DEBIT.dbt_typ.dbt_idtype.";
			Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws

                    Add 1 to Error_memo_count_ls
                    Set Failure_is in Debitside_screen_status_ls to true
                END-IF
                If Failure_is in Dsid_ret_stat   then
                    %^ Write an error memo indicating the problem
		    %beg
			field_ws = "DEBIT.dbt_typ.dbt_idtype";
			Mnemonic_ws =  "VMSG$_INVBANKSTR";
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                    Add 1 to Error_memo_count_ls
                    Set Failure_is in Debitside_screen_status_ls to true
                END-IF
		%^ If AUX, make to hookup
	        IF (Dbt_ovr of Dbt_typ of Ent_debit_set = "*" ) And
	    	   (Relget_msgcode = Vmsg_dat_notonfile_wc )  THEN
		   %beg
		    	Ent_debit_set (.Dbt_adr_set_ptr POINT: Relget_adr_set,
				       .flgs3.DBT_adr_ptr_ok = "T" );
	           %end
		End-if
	END-IF.


B260_DBT_MAP_END.


   EXIT.
C440_COPY_ADDRESS.
%^
	If (Adr_name_length of Relget_adr_set_lengths = ZERO )
	   AND (Dbt_name1_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name1 = NULL;  %End
	END-IF.
	If (Adr1_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name2_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name2 = NULL;  %End
	END-IF.
	If (Adr2_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name3_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name3 = NULL;  %End
	END-IF.
	If (Adr3_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name4_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name4 = NULL;  %End
	END-IF.
%^
%^ Copies address from Relget_adr_set to Debit party.  Also does zip code.
	%Beg
	Ent_debit_set (.Dbt_name1 = Relget_adr_set.adr_name,
		       .Dbt_name2 = Relget_adr_set.adr1,
		       .Dbt_name3 = Relget_adr_set.adr2,
		       .Dbt_name4 = Relget_adr_set.adr3,
		       .Dbt_adr_bnk_id = Relget_adr_set.bnk_id ) ;
%^ Build a DBT_ADR_TYPE field from country code and 
%^   ADR_TYPE field of Relget_adr_set
	Dsid_Compose ^OUT(Ent_debit_set.Dbt_adr_type),
		Relget_adr_set.Country_code(^STRING<2>),
		Relget_adr_set.Adr_type, /;
	%End.

%^ copy in the zip code too
	If (Zip of Relget_adr_set NOT = SPACES )
	    Call "ZIPSUB" Using
		by reference Dbt_name4 of Ent_debit_set
		by reference Dbt_name4_length of Ent_debit_set_lengths
		by reference Dbt_name3 of Ent_debit_set
		by reference Dbt_name3_length of Ent_debit_set_lengths
		by reference Zip of Relget_adr_set
		by reference Line_flg_ws

	    EVALUATE Line_flg_ws
		WHEN "4"
		    %Beg  Ent_debit_set.Dbt_name4 CHANGE;  %End

		WHEN "3"
		    %Beg  Ent_debit_set.Dbt_name3 CHANGE;  %End

	    END-EVALUATE
	END-IF.


C440_COPY_ADDRESS_END.
   EXIT.

D100_BCC_LKUP_BIC.

%^ If configured, replace "E" BCC Code with the BIC

%^ Assume successful processing
	Set Success_is in Bcc_lkup_status to true.

%^ Exit if not valid for processing 
	IF (Lkup_bic_by_bcc_ws = "D" OR "B") AND
	   (Relget_return_idtype = "E")
	    Continue
	ELSE
	    Go to D100_BCC_LKUP_BIC_END
	END-IF.

%^ If SWIFT ID not available, set error;
%^ Otherwise, replace BCC with BIC
%^ FTRSCR_EDITS will perform final validation
	IF Swift_id of Relget_adr_set = spaces 
	    Set Failure_is in Dsid_ret_stat to true
	    Add 1 to Dsid_bogus_parties
	    Add 1 to Error_memo_count_ls
	    Set Failure_is in Debitside_screen_status_ls to true
	    Set Failure_is in Bcc_lkup_status to true
	    %beg Dsid_return_key = NULL; %end
	    Evaluate Bcc_lkup_party
		When "OBK"
	    		%beg Field_ws = "DEBIT.OBK.OBK_ID"; %end
		When "ORP"
	    		%beg Field_ws = "DEBIT.ORP.ORP_ID"; %end
		When "SBK"
	    		%beg Field_ws = "DEBIT.SBK.SBK_ID"; %end
	    End-evaluate
	    %beg Mnemonic_ws = "LKUP$_SWF_ID_REQ" ; %End
	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws


	ELSE
	    %Beg
	    Relget_return_idtype = "S";
	    Dsid_compose ^OUT(Relget_return_key),
			Relget_adr_set.Swift_id, /;
	    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, ^SPACE, / ;
	    %End
	END-IF.

D100_BCC_LKUP_BIC_END.
   EXIT.


* Utility paragraphs.

X920_INFO_MEMO.
*  This paragraph writes an informational trace memo using the text string
*      in Dsid_info_memo.
	If Dsid_info_memo_length = 0 
	    GO TO X920_INFO_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_info_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_info_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_info_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)
	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
X920_INFO_MEMO_END.
	EXIT.

X930_CHANGE_DEBIT.
	%^
	%ACE_IS Relget_adr_set connected giving Dsid_conn_stat;
	%^
	%Beg
	BREAK: Prulepty_rule_seq(NOMOD);
	BREAK: Prulepty_party_adr_set(NOMOD);
	%End
	If Success_is in Dsid_conn_stat Then
		%beg Prulepty_source rel_is;
		     Prulepty_party_adr_ok Success_is;
	     	     Relget_adr_set.adr_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Relget_adr_set EQUATE: Prulepty_party_adr_set(NOMOD);
		%end
		If (Rel_id of Relget_adr_set = 0 )
		    %Beg  Prulepty_source aux_is;  %End
		end-if
	Else
		%beg Prulepty_source host_is;
		     Ent_Debit_set.Dbt_proc_rule CONN:
						       Prulepty_rule_seq(NOMOD);
		     BREAK: tmp_party_seq;
		     Prulepty_party_adr_ok Failure_is;
		%end
		Initialize Prulepty_party_adr_set
	end-if.

	%beg
	    Dsid_id_bank_ws = Ent_debit_set.dbt_adr_bnk_id;
	    Prulepty_bank_prof_rec = NULL;
	%end
	If (Dsid_id_bank_ws = SPACES)
	    %Beg  Dsid_id_bank_ws = Ent_ftr_set.Loc_info.Bank;  %End
	end-if

	If (Dsid_id_bank_ws = Bnk_id of Menu_bnk_union)
		%beg
		     Prulepty_bank_prof_rec =
					     Menu_bnk_union.Bnk_profile_id_rec;
		%end
	else
		%Beg
		   BREAK: Dsid_bnk_union ;
		   SEARCH: Bnk_index 
			   (Key = Dsid_id_bank_ws );
		%end
 		If (Success_is in Bnk_index_status)
			%Beg Bnk_index CONN: Dsid_bnk_union(NOMOD); %end
		end-if
		%beg
  		   Prulepty_bank_prof_rec = dsid_bnk_union.Bnk_profile_id_rec;
		%End
	end-if.

	Call "PRULE_CHANGE_DEBIT" returning Dsid_ret_stat.

X930_CHANGE_DEBIT_END.
	EXIT.


%^******************************************************************************
%^
%^      DEBIT_LOOK_ACCOUNT
%^
%^******************************************************************************
%^
%^ Desired new features:
%^
%^	TODO - 
%^
%^
%^******************************************************************************
%^
%^ DEBIT_LOOK_ACCOUNT routine.
%^
%^   Calling format:
%^	Call Debit_look_account" Using
%^              By Reference Init_debit_currency
%^		By Reference Debit_currency
%^		By Reference Message_amount
%^              By Reference Message_currency
%^		By Reference Debit_account
%^		By Reference Is_payment
%^		By Reference Xbank_account_ok
%^		By Reference Account_type
%^		By Reference Resume_SIs
%^		By Reference Is_repetitive_lookup
%^		By Reference Nochange_bank
%^		By Reference Lock_dbt_party
%^		By Reference Special_fee_key
%^		By Reference Debit_internal_state
%^		By Reference Currency_found
%^		By Reference Nothing_suspicious
%^		By Reference Msg_bank_changed
%^		By Reference Error_Memo_count
%^		By Reference Last_error_memo_text
%^		By Reference Last_error_memo_size
%^	    Giving Return_status_ws.
%^	Possible return status values are:
%^		SUCCESS
%^		FAILURE
%^
%^
%^      Called from DEBITSIDE_LOOKUP and from CREDITSIDE_LOOKUP to process the
%^   debit party AIN SIs, set up the final debit party information, set up the
%^   debit account, and set up the debit account information.
%^	The Ent_d_adr_set is assumed to be connected if the initial debit
%^   party found by the DEBITSIDE_LOOOKUP routine is on file.  We begin by
%^   checking the Debit_locked argument; if it is zero we process AIN SI's
%^   pushing down or overwriting the debit party if appropriate.  Once
%^   we have found the debit party, the utility SET_DEBIT_ADDRESS
%^    routine in SIDE_SUBS.COB is called to set debit party elements from 
%^    the address set.                   
%^
%^       We now proceed to debit account setup.  If the Debit_account argument
%^    is not SPACES, we will use it as the debit party's account REGARDLESS of
%^    the identity of the debit party.  If the Debit_account is not in the
%^    currency of the Debit_currency argument,  we will return an error.
%^    If the debit account argument is SPACES, then we will use the debit 
%^     address (if we were able to determine one) as the basis for finding a 
%^    debit account as follows:
%^	  If the Xbank_account_ok argument is zero, we will restrict the
%^	debit account search to finding accounts in the same bank as the debit
%^      party address.
%^        If the address was selected via an account id (type "D" OR "G" OR
%^      "F" OR "V" OR "P") then find that account and check its currency against
%^      the Debit_currency.  (NOTE: "P" is only allowed if the source was CHP.)
%^        If they do not match, give an error.
%^	  If they do match and ambiguity checking is not suppressed, check for
%^      a possible ambiguous account with the same identifier and the same 
%^      currency belonging to the same address.
%^	  Else if the address was NOT selected via an account id, check the
%^      Debit_currency argument.  If it is blank, look for an account in the
%^      message currency if that is not also blank.
%^	  Otherwise, look for the default account.  If there is one, check its 
%^      currency against the debit currency.  If they match, there can be no 
%^      ambiguity since there is only one default account.  If they do not
%^	match, scan the account sequence.
%^	  The account sequence scan goes through the account sequence skipping
%^      entries which do not have a matching currency.  If a match is found
%^      and ambiguity checking is not suppressed, the scan continues to see
%^      if there is another match.
%^        If we are able to unambiguously determine a debit account, we call the
%^    SET_DEBIT_ACCOUNT routine in SIDE_SUBS.COB to set debit set elements from
%^    the account set.  
%^        We will then execute any SI's which have been deferred to this point.
%^	  If any significant database anomalies were found in processing the
%^    debit party, the status is returned as FAILURE.  Otherwise it will be
%^    returned as SUCCESS.
%^        If the initial debit party was on file and the account and address
%^    information were found without incident, the Nothing_suspicious argument
%^    will be returned non-0 to indicate that this message may continue
%^    through automated payment processing.
%^
%^ INPUT ARGUMENTS:
%^ explicit:
%^  Init_Debit_currency       STR(3)  contains the initial debit currency.
%^      This is a mandatory field.
%^  Debit_currency	      STR(3)  contains the debit currency.
%^	This is a mandatory field.
%^  Message_amount	      DEC(14.2)  contains the message amount.
%^	This is a mandatory field.
%^  Message_currency          STR(3)  contains the message currency.  This
%^      is the currency trigger for SI's.
%^  Debit_account	      ACC_ID_REC.DDF contains account ID if caller
%^  	has pre-determined it; else spaces.
%^  Is_payment		      Long   If NON-0, message is a payment and 
%^	both an unambiguous debit address and debit account are required.
%^  Xbank_account_ok	      Long   Non-0 if we are permitted to find a
%^	debit account in a different bank from the debit party.
%^  Account_type	      STR(1) Select an account of this type from
%^ 	the accounts associated with the debit party address.
%^  Resume_SIs		      Long   Non-0 if we are resuming SI processing
%^      after interrupting ourselves to handle an immediate action SI such
%^	as a COR.
%^  Is_repetitive_lookup      Long   Non-0 if this is a repetitive lookup,
%^	in which case we will not map special instructions nor copy the
%^	debit account's cnf_seq to the message.
%^  Nochange_bank	      Long   Non-0 if a debit party bank which 
%^	disagrees with the Menu_bnk_union bank should not cause a bank
%^	context switch.  (The Msg_bank_changed flag will still be hoisted
%^	when appropriate even though no context switch will be done.)
%^  Lock_dbt_party            Long   Non-0 if the debit party is locked
%^      (such as by instantiation of a locked repetitive) and should not be
%^ 	changed by AINs, INTRTL tables, or anything else.
%^  Special_fee_key	      Str(1) Passed to Set_debit_account.  
%^				     SPACE for nothing special.
%^				     For now, "W" if the fees are to be waived.
%^  Debit_internal_state      Long   Internal indicator of what state of 
%^	completion the pre-creditside part of debitside-lookup achieved.
%^
%^  
%^ implicit:
%^   Debit party:
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^   SBK:
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^   Menu_bnk_union.Id for current bank id.
%^
%^
%^ OUTPUT ARGUMENTS:
%^explicit:
%^  Currency_found            Str(3) is the debit currency found -- the
%^	explicit currency of the debit account, if any.  It will only be SPACES
%^	if no debit account was found.
%^  Nothing_suspicious	      long   is returned 0 if any mapping errors
%^	were detected during debitside setup, otherwise 1.  For payments,
%^	mapping errors are caused by any ambiguous debitside parties, a
%^	NOF debit party, or a non-existent or ambiguous debit
%^	account.  For non-payments, mapping errors are caused by any
%^	ambiguous debitside party or a NOF (but not non-existent) debit
%^	party.  If this flag is returned 0, the message should be routed to
%^	repair instead of being passed through for automated payment
%^	processing.
%^  Msg_bank_changed	      long   is returned non-0 if the debit party was
%^      found in a different bank, requiring the message (context) bank to
%^      change.
%^  Error_Memo_count          Long   is the number of error or warning
%^	memos added to the message history during debitside lookup. 
%^	Informational memos indicating normal SI parsing and execution are
%^	NOT error or warning memos.
%^  Last_error_memo_text      Str(80) is the returned text of the last error
%^	or warning memo added to the message history during debitside lookup.
%^  Last_error_memo_size      Word    is the number of characters of returned
%^	text of the last error or warning memo added to the message history
%^	during debitside lookup.
%^  Return_status_ws is SUCCESS if no obviously bad or inconsistent
%^  rel or aux file data was read during debitside setup, otherwise FAILURE.
%^
%^Implicit:
%^   Message history
%^   Debit party:
%^	Ent_debit_set.Dbt_adr_set_ptr
%^	Ent_debit_set.Dbt_rel_id
%^	Ent_debit_set.Dbt_adr_bnk_id
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_account
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^	Ent_debit_set.Dbt_acc_class
%^	Ent_debit_set.Dbt_acc_parent_code
%^	Ent_debit_set.Dbt_acc_prod_codes
%^	Ent_debit_set.Dbt_adr_class
%^	Ent_debit_set.Dbt_adr_type
%^	Ent_debit_set.Dbt_concen_acc
%^	Ent_debit_set.Dbt_currency
%^	Ent_debit_set.Dbt_department
%^	Ent_debit_set.Dbt_flag   
%^	Ent_debit_set.Dbt_recon_ref
%^	Ent_debit_set.Dbt_spc_inst1
%^	Ent_debit_set.Dbt_spc_inst2
%^	Ent_debit_set.Dbt_spc_inst3
%^	Ent_debit_set.Dbt_sys_of_rec
%^	Ent_debit_set.flgs.dbt_hold_flg
%^	Ent_debit_set.flgs.dbt_lim_flg  
%^	Ent_debit_set.flgs.dbt_ps_elig_flg
%^   SBK:
%^	Ent_debit_set.Sbk_adr_set_ptr
%^	Ent_debit_set.Sbk_adr_bnk_id
%^	Ent_debit_set.Sbk_rel_id
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk_adr_set_ptr
%^	Ent_debit_set.Obk_adr_bnk_id
%^	Ent_debit_set.Obk_rel_id
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp_adr_set_ptr
%^	Ent_debit_set.Orp_adr_bnk_id
%^	Ent_debit_set.Orp_rel_id
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^
%^
%^ Modification history:
%^
%^	Fred P. Isaacs	5-FEB-1996
%^		Initial version.
%^      Fred P. Isaacs  19-APR-1996
%^              Make returned memo count get fudged to 2 from 1 if there are
%^		info memos since error memo may not be last.
%^		Insulate id in calls to ACCT_LOOKUP and make sure any changes
%^		made to fill out the id are kept.
%^	Fred P. Isaacs  10-MAY-1996
%^              Bias Debit party account selection strongly towards "P" for
%^		source "CHP" and strongly away from it otherwise,
%^              Make sure that we do not find or connect to account if we are
%^		not a payment message.
%^      Fred P. Isaacs    15-MAY-1996
%^		Make sure that we leave account blank and ? in ovr for account
%^		map failure.
%^              Fixed typo in CHIPS lookup path in account find logic.
%^		Now always writes memo when cannot set up debit account.
%^	Fred P. Isaacs	17-MAY-1996
%^              Shortened error memos; made error /info memo write more robust.
%^	Fred P. Isaacs	22-MAY-1996
%^		Changed args for SI_FIRST_DEBIT.
%^		Changed DEBIT_LOOK_ACCOUNT args to have separate amount and 
%^		   currency passed by caller.
%^	Fred P. Isaacs	28-MAY-1996
%^              Removed deletion of department etc when reconnect fails.
%^	Fred P. Isaacs	31-MAY-1996
%^		Make sure that Relget_msgcode is cleared before each lookup
%^		step so we don't confuse an AUX lookup hit with a complete miss
%^	Fred P. Isaacs	20-JUN-1996
%^              Correct trap in debitside pushdown.
%^	Fred P. Isaacs	02-AUG-1996
%^              Indicate parse failure for pushdown error.
%^              Now behaves better for AIN trying to insert NOF party.
%^              Now objects to NOF account for on-file debit party,
%^		but leaves edits on NOF debit party to FTRSCR_EDITS.
%^	John R. Phelan	30-AUG-1996
%^		Clear out Dbt_recon_ref.  #19552
%^      John R. Phelan  30-AUG-1996
%^              Set the NOF Dbt_account correctly.  #19175, #19199, #19313.
%^      John R. Phelan  11-OCT-1996
%^              Fix trap when debitside AIN Alt/Acct is ambig or NOF.  #20935
%^      John R. Phelan   21-OCT-1996
%^              Bypass Debitside AIN's for source codes CHP and FED.  If an
%^              AIN is bypassed for this reason, a memo is written to
%^              the message history stating "Debit AIN bypassed because
%^              source code is xxx".  #21012
%^      Fred P. Isaacs    22-NOV-1996
%^              Makes sure that debitside default account is not a P account
%^              unless our source is CHPs. #21535
%^		Migrated to 4.2 by John Phelan.
%^      John R. Phelan    30-DEC-1996
%^              Clear out debit party name and address when the address
%^              changes from on-file to not-on-file.  #21817
%^      John R. Phelan    23-JAN-1997
%^              If there's no room to insert a preferred correspondent,
%^              don't overwrite the debit party.  Also, make sure
%^		address sets are always connected NOMOD.  #22302
%^      John R. Phelan   27-JAN-1997
%^              Add call(s) to SET_NOF_DBT_BNK_ID to determine not-on-file
%^              Dbt_adr_bnk_id based on various criteria.  #24397
%^      John R. Phelan   15-MAR-1997
%^              Change the search currency when the debit party is changed by
%^              an AIN.  #23745
%^      John R. Phelan   15-MAR-1997
%^              Clear out the Dbt_hold flag from the previous account whenever
%^              the Dbt_account field is cleared.  #24455
%^      John R. Phelan  19-JUN-1997  
%^              Break the Dsid_accdef_seq so it doesn't cause problems for the
%^              next message.  #29383
%^      John R. Phelan   23-JUL-1997
%^              If the first SI search fails, AND the address has no accounts,
%^              AND the debit_amount_currency_ls is NOT spaces, then use the
%^              debit_amount_currency_ls as the search currency for the
%^              second SI search.  #31449
%^
%^	A. Smith	28-Aug-1997
%^		For an incoming CHIPS or equivalent, if the debit account has
%^		already been identified but it is not a "P" account type, then
%^		assume the mapper wants that to be the account (this won't ever
%^		happen in the CHIPS environment, since the sending bank participant
%^		number is the only id that ever gets mapped to a payment debit party,
%^		but it can happen in other clearing houses with payments that are, for
%^		example, the logical equivalent of a FED 1031 drawdown.
%^	Fred P. Isaacs 28-Aug-1997	AMX 33173
%^		Make sure that Dsid_acc_seq gets broken after probing for 
%^		currency during AIN execution.
%^
%^	Fred P. Isaacs  4-SEP-1997	SPR 28833
%^		Currency changes.
%^
%^      John R. Phelan  3-FEB-1998
%^      35648   Changed to call the new Validate_pid_adv_type subroutine to
%^              determine the valid "P" source code, since this can now vary
%^              depending on bank.  This change is necessary for the Amex
%^              German Clearing Interface.
%^
%^	Fred P. Isaacs  3-MAR-1998	40074
%^	Cleaned up logic to recognize DSTATE_BAD status and act appropriately.
%^
%^	Fred P. Isaacs  16-MAR-1998	40556
%^	Return "nothing suspicious" non-zero if we did nothing.
%^
%^	Fred P. Isaacs 19-MAR-1998	 40649
%^		Make sure that account information is cleared if we change
%^		to an ABA and don't need an account.
%^
%^      John R. Phelan  13-APR-1998     41754
%^      Both Debitside_lookup and Debit_look_account were using the same
%^      field, Dsid_info_count.  Since Debitside_lookup can now call
%^      Debit_look_account directly, this was sometimes causing the
%^      info memo count to be doubled.  Debitside_lookup has been changed
%^      to use a field Dsid_adr_info_count and Debit_look_account uses
%^      Dsid_acc_info_count so they don't interfere with each other.
%^      Dsid_dbt_party_error has also been split into two fields for the
%^      same reason.
%^
%^	Fred Isaacs	10-SEP-1998	#45875
%^ 	Now changes ID in message to match debit account.
%^
%^      Jan Walsh       02-FEB-1999     #51094
%^      Src_code "SEC" treated the same as src_code "FED".
%^
%^	J. Walsh    06-May-1999		SPR 15526, PROJ 54097
%^	Lookup fails if address is marked for deletion.
%^
%^	Fred P. Isaacs	14-Oct-1999	58119 for port
%^	PID suffix will be checked on suffixed PID against source;
%^	suffix for source will be appended onto unsuffixed PID.
%^
%^	Fred P. Isaacs	15-Oct-1999	58231 for port
%^	Cleaned up Dsid_acc_id_ws usage and pushdowns.
%^
%^	Fred P. Isaacs	1-Nov-1999	57940 for port
%^	Initialize Currency_found_ls argument.
%^
%^	B. Lanza	8-Jun-2000	#62674
%^	Debit acc not found.  Migrated following change from 4.3.
%^         Fred Kelley     31-Jul-1998   43062 43063
%^         When looking up account and account not found, try for equivalent
%^         currency. If found, call b280 a second time for new currency
%^
%^	Fred Isaacs	26-DEC-2000	70600
%^	Fix lookup logic for P accounts in DEBIT_LOOK_ACCOUNT.
%^
%^	B. Lanza	16-Feb-2001	71057
%^	Debit Concentration account was not being filled in debit set.
%^	Add REG: and BREAK: for Ent_d_acc_rel_reg whenever the
%^	ent_d_acc_set is conn: or break: 
%^
%^	Fred P. Isaacs  6-AUG-2001    74825, 76147
%^		Made "Y" accounts non-candidates for selection by currency
%^		match.
%^
%^	K Bjelke	9-Oct-2001	#77043
%^		Extra IF statement had disabled Default account lookup.
%^		Problem corrected.
%^
%^	Ken Bjelke	28-Apr-2002	83550
%^	In B200_get_newparty added check for AIN replacing of account with same.
%^	This was occurring when the original and the replacement account share the
%^	same address. Account was filled in correctly, but error status was returned
%^	to caller due to too loop count exceeded.
%^
%^
%^	Ken Bjelke  11-Sep-2002	spr 88276
%^	
%^	For Clearing House items, set the account to the GL provided by the
%^	Channel. Ensure that the DBT_ID is retained.
%^
%^ Fred P. Isaacs  11-NOV-2002  90950
%^		Changed Prule_party_fsect to make AUX prules work.
%^
%^	Bob Lanza	30-Jan-2003	94118
%^	When its a clearinghouse, search acc_seq looking for "P" account with suffix
%^	that matches the clearinghouse suffix. Added logic in B280_FIND_ACCOUNT.
%^
%^	B. Lanza	12-Mar-2003	96468, 91584
%^	Duplicate account error on debit party. Address for dbt party had 2 
%^	accounts (1 in USD and 1 in foreign currency). SWF input msg in foreign 
%^	currency that has ABA for cdt party was unable to determine which acct to 
%^	choose. Change made in A100_GOT_DEBIT, for baseline currency, added "A" to 
%^	cdt_typ's that allow message currency to be used as debit currency, which 
%^	then determines the account to use.
%^
%^
%^ Ken Bjelke 	24-Mar-2003
%^	Ensure that all proper address connections are maintained to retreive
%^	account numbers.
%^
%^ Ken Bjelke 	13-May-2003	99096,99068
%^	Use the account form the Channel record in all cases. Was only being
%^	used for Not on Rel accounts. This was not correct.
%^
%^ Ken Bjelke   27-May-2003	100496
%^	Correct account number choosing order.
%^
%^ Joanne Curley 1-Jul-2003	101709
%^	Corrected problem finding debitside nostro account.  When override
%^	account type is passed in, use that to find account instead of always
%^	looking for DDA account type.
%^
%^ Ken Bjelke	11-Jul-2003	102251
%^	More fun with finding Nostro accounts. Be sure to always use override
%^	when Passed in.
%^
%^ Fred P. Isaacs  7-AUG-2003  103339
%^	Changed level passed back by PRULE_MSG_RULE_MATCH to Dsid_msgpr_level
%^	from Dsid_pr_level because it's the Prule_msglevel_oneof level type.
%^
%^ Ken Bjelke 	14-Aug-2003	103339, 103528
%^		Correct calling of PRULE_CHANGE_DEBIT. AINs were not being found.
%^
%^ Ken Bjelke	07-Jan-2004	108776
%^	Only prefix DBTAIN SUB parties with bank IF the dbt_adr_bank is 
%^	Different that thebank of the new party.
%^
%^ Ken Bjelke 	31-Mar-2004	111895
%^	 Inhibit the choosing of "L" accounts, as is done with
%^	 P and Y.
%^
%^ Ken Bjelke 	05-May-2004	112963
%^	Correct FIND_ACC_CUR paragraph. Was falsly finding an  account by the
%^	type flag not being reset when the Currency match failed.
%^
%^ Ken Bjelke 	12-May-2004	95185,97006
%^	Add call to "CUST_BILAT_ACCT" to populate Bilateral account fields
%^	when required.
%^
%^ Ken Bjelke 	28-Jul-2004	115418
%^	Allow for AIN's above the address Level to trigger when Not
%^	an accounting IDtype
%^
%^ Ken Bjelke 	24-Nov-2004	118654
%^	break dsid_accdef_seq on all cases. Was being left connected after a
%^	Success return, causeing a Trap.
%^
%^ Ken Bjelke 15-Dec-2004	118663
%^ 	Compare the entire proposed DBTAIN party with the entire current
%^	prior to deciding whether this is a circular reference. 118049 did
%^	not cover all cases.
%^
%^ Ken Bjelke 	02-Feb-2005  120320	
%^	Clear dbt party name and address prior to inserting after a DBTAIN.
%^	Info was left over if original was shorter than new.
%^
%^ Migration:
%^ 	Ken Bjelke 	15-Mar-2006	129534
%^		Add call to GET_DBT_IDTYP_CHAN to populate the debit with a good IDtype
%^		based upon the source.
%^
%^ Ken BJelke 	20-Dec-2006	128078
%^	SEPA  Change ? to * in dbt_ovr if we are able to get an Account from the
%^	RTGS source. SHould indicate that we have from the creditside debit call. Only for DDR's
%^	
%^ Ken Bjelke 	16-May-2007	137551
%^	Re-Add code to skip account checking for "SEC" Msg ANT items. 
%^
%^ Ken Bjelke 	14-Dec-2009	tc14932   - Cr 141229
%^	Inhibit DBTAIN's from triggering on RTGS and Clearing_house items.
%^	
%^ End revision history
%^******************************************************************************

%^******************************************************************************
%module DEBIT_LOOK_ACCOUNT;

%^*****************************************************************************
%^                                                                            *
%^ Paragraphs in this procedure are labeled as follows:                       *
%^                                                                            *
%^	A100_MAIN	     Subroutine entry point, dispatch, and return
%^	B200_GET_NEWPARTY    Tries to get a pushdown/substitute party.
%^	B240_SET_ADDRESS     Sets up message debit party from address info.
%^      B270_PERFORM_FIND_ACC  Calls B280 maybe twice if Equiv currency.
%^	B280_FIND_ACCOUNT    Selects proper account from party address.
%^	B320_SET_ACCOUNT     Sets up message debit party from account info.
%^	C400_FIND_ADR_CUR    Disambiguate address based on currency match.
%^	C440_COPY_ADDRESS    Copies address info into debit party.
%^	C480_CHECK_CURRENCY  Checks Side_acc_seq currency against message
%^      C500_CHECK_TYP_CURRENCY Checks account currency against message 
%^      C520_CHECK_DUPE      Checks Side_acc_seq for ambiguating account.
%^	C560_FIND_ACC_CUR    Scans Side_acc_seq for account with currency
%^	C600_PUSH_DEBIT      Pushes current debit party down.
%^      X900_ERROR_MEMO	     Writes an error or warning memo.
%^	X920_INFO_MEMO	     Writes an informational memo.
%^                                                                            *
%^                                                                            *
%^*****************************************************************************
%^ Macro definitions.
%^ %MAR
%^ .default	displacement,long
%^ %end
%^*****************************************************************************
%linkage
01  Init_debit_currency_ls		%Str(3);
01  Debit_currency_ls	  		%Str(3);
01  Message_amount_ls	  		%Amount;
01  Message_currency_ls   		%Str(3);
01  Debit_account_ls	  		%Str(34);
01  Is_payment_ls	  		%boolean;
01  Xbank_account_ok_ls	  		%boolean;
01  Account_type_ls  	  		%Str(1);
01  Resume_SIs_ls	  		%Long;
01  Is_rptv_lookup_ls	  		%boolean;
01  Nochange_bank_ls	  		%boolean;
01  Lock_dbt_party_ls	  		%boolean;
01  Special_fee_key_ls			%Str(1);
01  Internal_state_ls	  		%Long;
01  Currency_found_ls 	  		%Str(3);
01  Nothing_suspicious_ls 		%boolean;
01  Msg_bank_changed_ls	  		%boolean;
01  Error_memo_count_ls	  		%Long;
01  Last_memo_ls 	  		%Str(80);
01  Last_memo_leng_ls 	  		%Length;
01  Debit_look_account_status_ls	%Boolean;

%Procedure using Init_debit_currency_ls, Debit_currency_ls, Message_amount_ls,
		Message_currency_ls, Debit_account_ls, Is_payment_ls, 
		Xbank_account_ok_ls, Account_type_ls, Resume_SIs_ls, 
		Is_rptv_lookup_ls, Nochange_bank_ls, Lock_dbt_party_ls,
		Special_fee_key_ls, Internal_state_ls, Currency_found_ls,
		Nothing_suspicious_ls, Msg_bank_changed_ls, 
		Error_memo_count_ls, Last_memo_ls, Last_memo_leng_ls
	RETURNING  Debit_look_account_status_ls.
%^*****************************************************************************

%^*****************************************************************************
A100_MAIN.
*  If there's nothing more to do, do it quickly and leave.
	set Failure_is in Msg_bank_changed_ls to true.
	Move Internal_state_ls to Dsid_internal.
	If (DSTATE_BAD in Dsid_internal )
	    Set Failure_is in Debit_look_account_status_ls to true
	    Set Failure_is in Nothing_suspicious_ls to true
	    Move SPACES to Currency_found_ls
	    %EXIT PROGRAM
	END-IF.

	If (DSTATE_DONE in Dsid_internal )
%^ No further changes.  Just set up returns.
	    %Beg  Dsid_conn_ws = Ent_d_acc_set state.conn ;  %End
	    If (Dsid_conn_ws NOT = 0 )
		Move Currency_Id of Ent_d_acc_set to Currency_found_ls
	        If (Currency_found_ls = SPACES)
	        THEN
		    %Beg
		    BREAK: Dsid_bnk_union ;
		    SEARCH: Bnk_index 
				(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
		    %End
		    If Success_is in Bnk_index_status  
		        %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		        Move Base_currency_id of Dsid_bnk_union to 
							      Currency_found_ls
		    END-IF
	        END-IF
	    END-IF
	    Set Success_is in Debit_look_account_status_ls to true
	    Set Success_is in Nothing_suspicious_ls to true
	    %EXIT PROGRAM
	END-IF.

*  Initialize returned variables.
	Set Success_is in Debit_look_account_status_ls to true.
	Set Failure_is in Nothing_suspicious_ls to true.
	Move ZERO to Error_memo_count_ls.
	Move ZERO to Dsid_acc_info_count.
	Move ZERO to Last_memo_leng_ls.
	Move SPACES to Last_memo_ls.
        Set Failure_is in Dsid_debit_acc_erred to TRUE
	Set Success_is in Dsid_musthave_acct to true.
%^	Move ZERO to Dsid_push_error.
	Set Failure_is in Dsid_push_error to TRUE.
	Set Failure_is in Dsid_debit_pend_del to TRUE.
	%Beg  Dsid_know_acc_id = NULL;  %End.
	Move SPACES to Currency_found_ls.
	If (Debit_account_ls NOT = SPACES )
            Move Debit_account_ls to Dsid_instring
	    %Beg
	    Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
		Dsid_instring, / ;
	    Dsid_parse ^IN(Dsid_temp1_vstr)
		^OPTION( Dsid_know_acc_id.idbank(^STR<3>)),
		Dsid_intype_oneof( ^ONEOF(" ",
					"D",
					"G",
					"V",
					"F",
					"P" )),
		Dsid_know_acc_id.idkey(^STR<-1>), / ;
	   Dsid_compose ^OUT(Dsid_know_acc_id.idtype)
		Dsid_intype_oneof(^ONEOF(" ",
					"D",
					"G",
					"V",
					"F",
					"P" )), / ;
	   %End
	END-IF.
        Move spaces to Dsid_dupe_acc_ws.
        Move Debit_currency_ls to Dsid_currency_ws.

	Move SPACE to Dsid_ambig_ws 
	Move SPACE to Dsid_multibank_ws 
	Set Failure_is in Dsid_set_address to TRUE
	Set Failure_is in Dsid_set_account to TRUE
	Move SPACES to Dsid_preferred_corr
	%Beg  Dsid_preferred_corr = NULL ;  %End
	MOVE ZERO TO Dsid_si_suspended
	If DSTATE_INIT in Dsid_internal
	    Move 1 to Dsid_defer_adv_ws
	ELSE
	    Move ZERO to Dsid_defer_adv_ws
	END-IF

%^
%^ Get the channel, to see if it is RTGS to allow us to bypass DBTAIN inserts. 
%^
%^141229


	Set ID_IS in dsid_prchan_mode to True
	If Dbt_adr_bnk_id of Ent_debit_set not = spaces then
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_debit_set.dbt_adr_bnk_id;
		%End
	Else
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_ftr_set.Loc_info.Bank;
		%End
	End-if

	%Beg dsid_chan_ident_ws.idkey = Ent_ftr_set.src_code; %End
	Call "GET_CHANNEL" using
			By Reference 	dsid_prchan_mode
			By Reference	dsid_chan_ident_ws
			By Reference 	dsid_chan_ident_ws_lengths
	Returning dsid_got_channel_ws.


* NOTE: Resume_SIs_ls is ignored for now since it would just cause us to do
* 	everything that we now do.

	If Incoming_msgtype of Ent_ftr_set = "210"
	Then
	    Set Success_is in Dsid_210_ant_msg to TRUE
	Else
	    Set Failure_is in Dsid_210_ant_msg to TRUE
	END-IF.

	%Beg  BREAK: Dsid_acc_seq ;  %End.

	Move ZERO to Dsid_conn_stat.
	Move ZERO to Relget_msgcode.
	If( (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE) 
	    AND (Dbt_rel_id of Ent_debit_set NOT = 0 )) Or
	    (dbt_adr_ptr_ok of flgs3 of ent_debit_set = "T")	THEN
*  Hook up address set - we will need it.
	    %Beg   Dsid_conn_stat = Ent_d_adr_set state.conn ;  %End
	    If Dsid_conn_stat = 0
		If (Dbt_rel_id of Ent_debit_set NOT = 0 )
		    %ace_conn_root_q Rel_index ;
                    %Beg
		    BREAK: Relget_adr_set ;
		    Rel_index ^SEARCH (Key = Ent_debit_set.dbt_rel_id);
		    %end
		    If (Success_is in Rel_index_status   )
		       AND (OBJECT_IS in Rel_index_cursor )
		       AND (ADDRESS_IS in Rel_type of Rel_index)
                    THEN
		        %Beg  
			Rel_index CONN: Ent_d_adr_set(NOMOD) ;  
			Dsid_conn_stat = Ent_d_adr_set state.conn ;
		        %End
                    END-IF
		Else
			%^ AUX record, connect via ptr
			%beg
				Break: relget_adr_set;
				Ent_debit_set.dbt_adr_set_ptr CONN: ent_d_adr_set(nomod);
				dsid_conn_stat = ent_d_adr_set state.conn;
			%end
		END-IF
	    END-IF
	END-IF.
	If (Dsid_conn_stat = 0 )
* We really DON'T have a debit party.
            If Dbt_ovr of Dbt_typ of Ent_debit_set = spaces then
* But don't change ambiguous lookup into NOF lookup
                %Beg Ent_debit_set.dbt_typ.Dbt_ovr = "*" ; %End
            end-if
            IF (Dbt_rel_id of Ent_debit_set NOT = ZERO )
                %Beg
                Ent_debit_set( .dbt_name1      = NULL,
                               .dbt_name2      = NULL,
                               .dbt_name3      = NULL,
                               .dbt_name4      = NULL,
			       .dbt_res_country = NULL ) ;
                %End
                Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
                MOVE "N" to Dbt_comm_charge_ws
                MOVE "N" to Dbt_cbl_charge_ws
            END-IF
	    %Beg  
	    Ent_debit_set( .Dbt_rel_id 	       = <0> ,
			   .Dbt_adr_set_ptr DELETE,
			   .flgs3.dbt_adr_ptr_ok = Null) ;
	    BREAK: Ent_d_adr_set ;
            %End
            Call "SET_NOF_DBT_BNK_ID" using
		by reference Nochange_bank_ls
	        by reference Dsid_loc_bank_change
	      RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		set success_is in Msg_bank_changed_ls to true
	    END-IF
            If (Success_is in Msg_bank_changed_ls)
              AND (Success_is in Nochange_bank_ls)
              THEN
                %^ Write an error memo indicating the discrepancy.
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id,
                    " conflicts with message bank: "
                    Ent_ftr_set.loc_info.bank, / ;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
            If (Failure_is in Dsid_ret_stat  )
            THEN
                %^ Write an error memo indicating the problem
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Invalid bank in debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id, /;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
	    Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END
	    Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	    GO TO A100_MAIN_EXIT
	ELSE
	    %Beg
	    Ent_d_adr_set.account_seq CONN: Dsid_acc_seq (NOMOD) ;
	    BREAK: Relget_adr_set ;
	    Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
	    %End
	    Set Failure_is in Dsid_didansi_ws to TRUE
	    IF (Success_is in Lock_dbt_party_ls)
* Do not change the debit party
	        Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	        GO TO A100_MAIN_EXIT
	    END-IF
	    If (DSTATE_INIT in Dsid_internal)
		Move SPACES to Dsid_adv_type
	    ELSE
		Move Cdt_adv_typ of Ent_credit_set to Dsid_adv_type
	    END-IF
	    Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END
	    Perform B200_GET_NEWPARTY through B200_GET_NEWPARTY_END
	    If (Failure_is in Debit_look_account_status_ls   )
		OR (Dsid_si_suspended NOT = 0 )
	    THEN
	        Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	        GO TO A100_MAIN_EXIT
	    END-IF
	END-IF.
	
%^A100_PUSH.
	Move 4 to Dsid_push_count.
	Move ZERO to Relget_msgcode.
	Perform UNTIL (Idtype of Dsid_preferred_corr = SPACE )
%^		       OR (Dsid_push_error NOT = 0 )
		       OR (Success_is in Dsid_push_error )
		       OR (Dsid_si_suspended NOT = 0 )
	    If (Dsid_push_count > 0 )
	        Subtract 1 from Dsid_push_count
	    ELSE
	    	%Beg
		Dsid_compose ^OUT(Dsid_err_memo)
		       "Preferred correspondent loop?? for "
			Dsid_preferred_corr, / ;
	 	%End
		Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		Set Failure_is in Debit_look_account_status_ls to true
                Set Success_is in Dsid_debit_acc_erred to TRUE
		Set Success_is in Dsid_push_error to TRUE

	        Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	        GO TO A100_MAIN_EXIT

	    END-IF
	    Set DBT in Relget_title_flag to TRUE
	    Move SPACES to Dsid_lookup_temp
	    %Beg  Dsid_conn_stat = Ent_d_adr_set state.conn;  %End

	    Move "F" to Dsid_lkup_pend_del
 	    Call "ACCT_LOOKUP" using
	        by reference Idtype of Dsid_preferred_corr
	        by reference Idkey of Dsid_preferred_corr
		by reference Dsid_ovr
	        by reference Dsid_ambig_ws 
	        by reference Dsid_multibank_ws
	        by reference Debit_currency_ls
	        by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret_stat

            If Dsid_lkup_pend_del = "T"
	        Set Success_is in Dsid_debit_pend_del to TRUE
            End-if
	    If (Dsid_ovr = SPACE)
	        AND (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE )
	        AND (Rel_id of Relget_adr_set = Rel_id of Ent_d_adr_set )
		AND (Idtype of Dsid_preferred_corr NOT = 
				"D" AND "G" AND "V" AND "F" AND "P" AND "A" )
		AND (Dsid_lkup_pend_del NOT = "T" )
	    THEN
		%Beg
	        Dsid_compose ^OUT(Dsid_err_memo)
		    		"Debit party is its own preferred corr ",
				Dsid_preferred_corr, / ; 
	        %End
	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	        Set Failure_is in Debit_look_account_status_ls to true
                Set Success_is in Dsid_push_error To TRUE
                Set Success_is in Dsid_debit_acc_erred To TRUE

	    	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	    	GO TO A100_MAIN_EXIT

            ELSE
		If (Failure_is in Dsid_push_error )
	           AND (PUSHDOWN in Dsid_pushdown  )
	        THEN
		    %Beg  Dsid_conn_ws = Relget_adr_set state.conn ;  %End
		    If (Dsid_conn_ws NOT = 0 )
			%Beg
			BREAK: Dsid_adr_set ;
        		Relget_adr_set EQUATE: Dsid_adr_set(NOMOD) ;
			BREAK: Relget_adr_set ;
			%End
	            END-IF
		    If Dsid_conn_stat NOT = 0
			%Beg
			Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
			BREAK: Ent_d_adr_set;
	                %End
		      ELSE
			Move 0 to Rel_id of Relget_adr_set
		    END-IF
	            Perform C600_PUSH_DEBIT through C600_PUSH_DEBIT_END
		    %^ If we got a push error, break out of perform loop

		    If Success_is in Dsid_push_error then
	    		Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
 	    		GO TO A100_MAIN_EXIT
		    END-IF
		    %Beg  BREAK: Relget_adr_set ;  %End
                    If (Dsid_conn_ws NOT = 0 )
			%Beg
			Dsid_adr_set EQUATE: Relget_adr_set(NOMOD) ;
			BREAK: Dsid_adr_set ;
			%End
		    ELSE
%^ We must repeat the lookup.
		    	Move SPACES to Dsid_lookup_temp
			Move "F" to Dsid_lkup_pend_del
 	    		Call "ACCT_LOOKUP" using
		            by reference Idtype of Dsid_preferred_corr
		            by reference Idkey of Dsid_preferred_corr
	        	    by reference Dsid_ovr
		            by reference Dsid_ambig_ws 
		            by reference Dsid_multibank_ws
	        	    by reference Debit_currency_ls
	    		    by reference Dsid_lkup_pend_del
		          RETURNING Dsid_ret_stat
		    END-IF
	        END-IF

	        If (Dsid_ovr = SPACE )
		    OR ( ( Dsid_ovr = "*" )
		         AND (Relget_msgcode = Vmsg_dat_notonfile_wc ) )
	        THEN
* We actually did get a REL or AUX db hit, so let's copy the address.
	            Perform C440_COPY_ADDRESS through C440_COPY_ADDRESS_END
	        END-IF
                If (Dsid_ovr NOT = SPACE )
		    If (Dsid_ovr = "*" )
		        %Beg
	                Dsid_compose ^OUT(Dsid_err_memo)
		    		"NOF debit party preferred corr ",
				Dsid_preferred_corr, / ; 
	                %End
		    ELSE
			If (Success_is in Dsid_debit_pend_del)
	                    %Beg
	                    Dsid_compose ^OUT(Dsid_err_memo)
		    		"Debit party preferred corr ",
				Dsid_preferred_corr, " is pend delete.", / ; 
	                    %End
			ELSE
	                    %Beg
	                    Dsid_compose ^OUT(Dsid_err_memo)
		    		"Ambiguous debit party preferred corr ",
				Dsid_preferred_corr, / ; 
	                    %End
			END-IF
		    END-IF

	            Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	            Set Failure_is in Debit_look_account_status_ls to true
                    Set Success_is in Dsid_push_error To TRUE
                    Set Success_is in Dsid_debit_acc_erred To TRUE
	            %Beg
	            Ent_debit_set(.Dbt_typ 
				   (.Dbt_ovr    = Dsid_ovr,
				    .Dbt_idtype = Dsid_preferred_corr.Idtype,
				    .Dbt_id     = Dsid_preferred_corr.Idkey ),
				  .Dbt_name1 = NULL,
				  .Dbt_name2 = NULL,
				  .Dbt_name3 = NULL,
				  .Dbt_name4 = NULL,
				  .dbt_res_country = NULL) ;
	            %End
                    Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
                    MOVE "N" to Dbt_comm_charge_ws
                    MOVE "N" to Dbt_cbl_charge_ws
	            Call "SET_NOF_DBT_BNK_ID" using
			by reference Nochange_bank_ls
		        by reference Dsid_loc_bank_change
		      RETURNING Dsid_ret_stat
		    If (Dsid_loc_bank_change NOT = 0 )
			set success_is in Msg_bank_changed_ls to true
		    END-IF
	            If (Success_is in Msg_bank_changed_ls)
        	      AND (Success_is in Nochange_bank_ls)
	              THEN
        	        %^ Write an error memo indicating the discrepancy.
                	%Beg
	                Dsid_compose ^OUT(Dsid_err_memo)
        	            "Debit party: ",
                	    Ent_debit_set.dbt_typ.dbt_idtype,"/",
	                    Ent_debit_set.dbt_typ.dbt_id,
        	            " conflicts with message bank: "
                	    Ent_ftr_set.loc_info.bank, / ;
	                %End
        	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	            END-IF
        	    If (Failure_is in Dsid_ret_stat  )
	            THEN
        	        %^ Write an error memo indicating the problem
                	%Beg
	                Dsid_compose ^OUT(Dsid_err_memo)
        	            "Invalid bank in debit party: ",
                	    Ent_debit_set.dbt_typ.dbt_idtype,"/",
	                    Ent_debit_set.dbt_typ.dbt_id, /;
        	        %End
                	Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	            END-IF
	        ELSE
		    If (Relget_return_key NOT = SPACES )
%^ Update id with what we actually found.
	    	        %Beg
	    	        Dsid_parse ^IN(Relget_return_key)
		    	 	Dsid_preferred_corr.idkey, ^SPACE, / ;
	    	        %End
		    END-IF		
		    IF (Success_is in Dsid_didansi_ws)
		        %Beg
		        Dsid_compose ^OUT(Dsid_info_memo)
				"Debit party ",
				Ent_debit_set.dbt_typ,
				Dsid_pushdown (^oneof(" Rule substituted ", 
						      " Rule inserted ")),
				Dsid_preferred_corr.Idtype,"/",
			        Dsid_preferred_corr.Idkey, / ;
		        %End
		        Perform X920_INFO_MEMO through X920_INFO_MEMO_END
		    END-IF

 	            %Beg
 	            Ent_debit_set.dbt_typ 
				   (.Dbt_ovr    = Dsid_ovr,
				    .Dbt_idtype = Dsid_preferred_corr.Idtype,
				    .Dbt_id     = Dsid_preferred_corr.Idkey );
		    BREAK: Ent_d_adr_set;
                    Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;
		    Dsid_conn_stat = <1>;
                    Ent_debit_set(  .Dbt_rel_id = Ent_d_adr_set.Rel_id,
                                    .Dbt_adr_set_ptr POINT: Ent_d_adr_set,
			   	    .flgs3.dbt_adr_ptr_ok = "T" );
		    %End
		    Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END
	            Perform B200_GET_NEWPARTY through B200_GET_NEWPARTY_END
		END-IF
	    END-IF		    
	END-PERFORM.

* We have either pushed down through all preferred correspondents or had
* an error doing so.

	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END.

A100_MAIN_EXIT.
* Here we format the subsidiary debitside parties.
	If ( Orp_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Orp_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Orp.Orp_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.orp.orp_id)
			Ent_debit_set.Orp_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

	If ( Obk_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Obk_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Obk.Obk_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.obk.obk_id)
			Ent_debit_set.Obk_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.
	If ( Sbk_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Sbk_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_parse ^IN(Ent_debit_set.Sbk.Sbk_id)
			^SPACE, ^OPTION(^STRING, ":"), Dsid_id_ws, ^SPACE, / ;
	    Dsid_Compose ^Out(Ent_debit_set.sbk.sbk_id)
			Ent_debit_set.Sbk_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

	If Failure_is in Debit_look_account_status_ls  
	    Set DSTATE_BAD in Dsid_internal to TRUE
	ELSE
	    If (Dsid_si_suspended NOT = 0 )
		Set DSTATE_HOLD in Dsid_internal to TRUE
	    ELSE
		Set DSTATE_DONE in Dsid_internal to TRUE
	    END-IF
	END-IF.
	%Beg  Dsid_intern_word = Dsid_internal ;  %End
	Move Dsid_intern_word to Internal_state_ls

%^	If (Dsid_push_error = 0 )
	If (Failure_is in Dsid_push_error )
	THEN
* Debit party was completely processed.
	    Set success_is in Nothing_suspicious_ls to true
	END-IF.
	If (Success_is in Is_payment_ls)
* For non-payments, we don't require a debit account
	    If ( NOT ( (Dbt_idtype of dbt_typ of Ent_debit_set = "A" )
               AND (Src_code of Ent_ftr_set = "FED" OR "SEC" ) ) )
	    THEN
	        If (Failure_is in Dsid_set_account)
		   AND (Success_is in Dsid_musthave_acct   )
	        THEN
		    If (Success_is in Dsid_set_address)
* It is inconsistent.  We have an address, but no account.
			If (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = "?" )
			    %Beg  
			    Ent_debit_set.dbt_typ.dbt_ovr = "?" ;
			    Dsid_compose ^OUT(Dsid_err_memo)
				"No debit account for debit party "
				Ent_debit_set.dbt_typ, / ;
		            %End
	                    Perform X900_ERROR_MEMO through 
							X900_ERROR_MEMO_END
			END-IF
		        Set Failure_is in Nothing_suspicious_ls to true
		    END-IF
		END-IF
	    END-IF
	END-IF.

	If (Dsid_err_memo_length NOT = 0 )
            Move Dsid_err_memo_length to Last_memo_leng_ls
            Move Dsid_err_memo(1:Dsid_err_memo_length) to Last_memo_ls
        END-IF.

	%Beg
	BREAK: Dsid_acc_seq ;
	%End.
* Info memo could obscure the error memo so make sure that they get
*     the multiple memo.
	Add Dsid_acc_info_count to Error_memo_count_ls.


%^ DEBITSIDE_LOOKUP and DEBITSIDE_LOOK_ACCOUNT should only return
%^ Nothing_suspicious_ls as 0 and/or their returned stati as NOT SUCCESS if the
%^ lookup error occurred on the SBK party address or the debit party address if
%^ there is no SBK party address and their Is_payment_ls argument is 0.
%^ (Errors on the ORP or OBK debitside parties when the Is_payment_ls argument
%^ is =0 should behave the same as CREDITSIDE_LOOKUP errors and
%^ Nothing_suspicious_ls should be returned set to 1 and the stati should
%^ be returned as SUCCESS.)
        If (Failure_is in Is_payment_ls)
                If (Failure_is in Dsid_debit_acc_erred)
                   OR Sbk of Ent_debit_set NOT = SPACES
                   OR Sbk_name1 of Ent_debit_set NOT = SPACES
                   OR Sbk_name2 of Ent_debit_set NOT = SPACES 
		THEN
                    Set Success_is in Nothing_suspicious_ls to true
                    Set Success_is in Debit_look_account_status_ls to true
		END-IF
        END-IF.

        If Dbt_ovr of Ent_debit_set not = spaces then
                Call "SET_NOF_DBT_ACCOUNT"
        end-if.
	
 	%^
	%^ Update Debit Party
	%^
	%beg dsid_upd_level dbt_party_is; %end
	Call "PRULE_UPDATE_PARTY" Using
		By Reference  dsid_upd_level
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.


A100_MAIN_END.
        %EXIT PROGRAM.


%^ new paragraph - code moved from A100_main.
A110_GOT_DEBIT.
	Move SPACES to Currency_found_ls
	IF (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = SPACE )
* NOF debit party.  Set debit party bank to something reasonable.
	    If (Dbt_rel_id of Ent_debit_set NOT = 0 )
		%Beg
		Ent_debit_set( .Dbt_name1          = NULL ,
			       .Dbt_name2          = NULL ,
			       .Dbt_name3          = NULL ,
			       .Dbt_name4          = NULL ,
			       .dbt_res_country    = NULL) ;
		%End
                Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
	        MOVE "N" to Dbt_comm_charge_ws
	        MOVE "N" to Dbt_cbl_charge_ws
	    END-IF
            Call "SET_NOF_DBT_BNK_ID" using 
		by reference Nochange_bank_ls
                by reference Dsid_loc_bank_change
              RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		set success_is in Msg_bank_changed_ls to true
	    END-IF
            If (success_is in Msg_bank_changed_ls)
              AND (Success_is in Nochange_bank_ls)
              THEN
                %^ Write an error memo indicating the discrepancy.
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id,
                    " conflicts with message bank: "
                    Ent_ftr_set.loc_info.bank, / ;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
            If (Failure_is in Dsid_ret_stat  )
            THEN
                %^ Write an error memo indicating the problem
                %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
                    "Invalid bank in debit party: ",
                    Ent_debit_set.dbt_typ.dbt_idtype,"/",
                    Ent_debit_set.dbt_typ.dbt_id, /;
                %End
                Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            END-IF
	    If (Dbt_ovr of Dbt_typ of Ent_debit_set = "*")
		AND (Fx_ena of Menu_cfg = LOW-VALUES )
	    THEN
		%Beg
		BREAK: Dsid_bnk_union ;
		SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
		%End
		If Failure_is in Bnk_index_status  
		    Move SPACES to Currency_found_ls
		ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Base_currency_id of Dsid_bnk_union to 
			Currency_found_ls
		END-IF
	    END-IF
	    %Beg
	    Dsid_internal DSTATE_DONE ;
	    Dsid_intern_word = Dsid_internal ;
	    %End
	    Move Dsid_intern_word to Internal_state_ls
	ELSE
	    Perform B240_SET_ADDRESS through B240_SET_ADDRESS_END
	END-IF.

* Just in case we have a debit party bank change and context switches
*  are disabled.
	If ( Dbt_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Dbt_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_id_ws = Ent_debit_set.Dbt_typ.Dbt_id;
	    Dsid_Compose ^Out(Ent_debit_set.dbt_typ.dbt_id)
			Ent_debit_set.Dbt_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

	%Beg
	Dsid_dupe_acc_ws = NULL ; 
	%End.

%^	Ensure that we fill in res_country if possible

	If dbt_res_country of ent_debit_set = spaces
	    Initialize Dsid_risk_country_ws, Dsid_country_code_ws
	    If rel_id of relget_adr_set not = 0
		  %Ace_is relget_adr_set connected;
		  If success_is in ace_status_wf
	            Move risk_country of relget_adr_set to Dsid_risk_country_ws
	            Move country_code of relget_adr_set to Dsid_country_code_ws
		  end-if
	    end-if
	    Call "DETERM_RES_COUNTRY" using
	       by content "DBT"
	       by reference dbt_idtype of Ent_debit_set
	       by reference dbt_id of Ent_debit_set
	       by reference dbt_id_length of Ent_debit_set_lengths
	       by reference Dsid_risk_country_ws
	       by reference Dsid_country_code_ws
	       by reference dbt_res_country of Ent_debit_set
	       by reference Dsid_res_country_ws
	    If dsid_res_country_ws not = spaces
	        %Beg Ent_debit_set.dbt_res_country = Dsid_res_country_ws; %end
	    end-if
	end-if.

	Call "GET_DBT_IDTYP_CHAN" Using
		By Reference Dsid_temp_idtype,
		By Reference Dsid_lookup_temp,
		By Reference Dsid_lookup_temp_length
	Returning dsid_ret_stat.

	If Success_is in dsid_ret_stat Then
		%^ have have a good ID use it
		%beg ent_debit_set.dbt_typ.dbt_idtype = Dsid_temp_idtype;
  		     Dsid_compose ^OUT(Ent_Debit_set.dbt_typ.dbt_id)
				    Dsid_lookup_temp,/;
		%end
	end-if.

	If (Failure_is in Is_payment_ls)
* No reason to find or connect the account.
	    GO TO A110_GOT_DEBIT_END
	END-IF.

	%Beg  
	BREAK: Ent_d_acc_set ; 
	BREAK: Ent_d_acc_rel_reg;
	 %End
	If (Dsid_know_acc_id = SPACES )
* We have not been passed a debit account, so we should try to find one.

	%^ Need to clean this up, the calls are redundant.
	    Perform B300_GET_ACC_FROM_CHAN thru
		    B300_GET_ACC_FROM_CHAN_END
	    If (dsid_know_acc_id not = Spaces) Then
		  %^ Item came from a Clearing System, use it's Account
	          GO TO A110_GOT_DEBIT_CLEANUP
	    end-if
		
 	    If (Dbt_rel_id of Ent_debit_set = 0 )
* NOF debit account. Nothing to do
		Go tO A110_got_debit_end
	    ELSE
		%^ bring back to handle SEC Anticipations 137551
		If (Dbt_idtype of Dbt_typ of Ent_debit_set = "A" )
		    AND (Src_code of Ent_ftr_set = "SEC"  And
			 tran_type of ent_ftr_set = "ANT")
		THEN
		%^ It's an incoming fed, so we don't need an account.
				    Set Failure_is in Dsid_musthave_acct to true
				    Move "USD" to Currency_found_ls
				    Set DSTATE_DONE in Dsid_internal to TRUE
				    %Beg  Dsid_intern_word = Dsid_internal ;  %End
				    Move Dsid_intern_word to Internal_state_ls
				    GO TO A110_GOT_DEBIT_END
		END-IF 	%^------------- End 137551
%^ We have a debit address, so let's go look up an account.
                If (Debit_currency_ls NOT = SPACES )
%^ Search currency was set by caller
                    Perform B270_PERFORM_FIND_ACC     through
                            B270_PERFORM_FIND_ACC_END
		ELSE
		    If Message_currency_ls = SPACES 
                        Perform B270_PERFORM_FIND_ACC     through
                                B270_PERFORM_FIND_ACC_END
		    ELSE
%^ Message currency is set but debit currency was not.
			If (Dbt_idtype of Dbt_typ of Ent_debit_set =
					      "D" OR "V" OR "G" OR "P" OR "F" )
                            Perform B270_PERFORM_FIND_ACC     through
                                    B270_PERFORM_FIND_ACC_END
			ELSE
%^ Not an account identifier
        	            If (Fx_ena of Menu_cfg = LOW-VALUES )
%^ Baseline currency environment
				If (Cdt_idtype of Cdt_typ of Ent_credit_set =
						     "D" OR "V" OR "G" OR "P" OR
				 	(Cdt_idtype of Cdt_typ of Ent_credit_set = "A" 
				   	  AND Base_currency_id of Menu_bnk_union = "USD") )
				    Move Message_currency_ls to Dsid_currency_ws
                                ELSE
				    If (Cdt_idtype of Cdt_typ of 
							Ent_credit_set = "F" )
				        %Beg
				        BREAK: Dsid_bnk_union ;
				        SEARCH: Bnk_index 
					 (Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
				        %End
				        If Success_is in Bnk_index_status  
		    		            %Beg  
				            Bnk_index CONN: 
							Dsid_bnk_union(NOMOD) ;
				            %End
		    		            Move Base_currency_id of 
						    Dsid_bnk_union
							 to Dsid_currency_ws
					END-IF
				    END-IF
				END-IF
                                Perform B270_PERFORM_FIND_ACC     through
                                        B270_PERFORM_FIND_ACC_END
                           ELSE
%^ Cross-currency environment.  Try the message currency.
			        Move Message_currency_ls to Dsid_currency_ws
                                Perform B270_PERFORM_FIND_ACC     through
                                        B270_PERFORM_FIND_ACC_END
			        If (Dsid_know_acc_id = SPACES )
%^ That didn't work.  Try no preference and defaulting.
			            Move SPACES to Dsid_currency_ws
                                    Perform B270_PERFORM_FIND_ACC     through
                                            B270_PERFORM_FIND_ACC_END
			        END-IF
			    END-IF
		        END-IF			    	
		    END-IF
		END-IF
	    END-IF
        ELSE
%^ We were passed a debit account
	    If (Debit_currency_ls NOT = SPACES )
		%ace_conn_root_q Rel_acc_index ;
		%Beg
		SEARCH: Rel_acc_index (FORWARD, GEQ, .Rel_name_key 
				(.Idbank        = Dsid_know_acc_id.Idbank,
				 .Idtype        = Dsid_know_acc_id.Idtype,
				 .Idkey (.Idacc = Dsid_know_acc_id.Idkey,
				         .Idadr = NULL,
			       		 .Idpad = NULL ) ) ) ;
                %End
	        If (Success_is in Rel_acc_index_status   )
	           AND (Idbank of Rel_name_key of Rel_acc_index =
						Idbank of Dsid_know_acc_id )
	           AND (Idtype of Rel_name_key of Rel_acc_index =
						Idtype of Dsid_know_acc_id )
	           AND (Idacc of Idkey of Rel_name_key of Rel_acc_index =
						Idkey of Dsid_know_acc_id )
	        THEN
		    %Beg  
		    Rel_acc_index CONN: Ent_d_acc_set(
					REG:  Ent_d_acc_rel_reg (NOMOD)); 
		    %End
                    If (Debit_currency_ls not = SPACES AND
						Currency_id of Ent_d_acc_set)
			AND ( (Debit_currency_ls not =
					Base_currency_id of Menu_bnk_union)
			      OR (Currency_id of Ent_d_acc_set NOT = SPACES) )
		    THEN
                        %^ Mismatch.
		        %Beg
		        Dsid_compose ^OUT(Dsid_err_memo)
			    "Requested debit account ", Dsid_know_acc_id, 
			    " not set; not in currency ", Dsid_currency_ws, / ;
	        	%End
	        	Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
			Move SPACES to Dsid_know_acc_id
		    END-IF
		END-IF
            END-IF
	END-IF.
	If (Dsid_know_acc_id = SPACES )
* Still couldn't resolve debit account.
	    Move SPACES to Dbt_account of Ent_debit_set
	    %Beg
	    Ent_debit_set.dbt_account CHANGE ;
	    Ent_debit_set.dbt_typ.dbt_ovr = "?" ;
            Dsid_compose ^OUT(Dsid_err_memo)
		   "Could not find debit account for address ",
				Ent_debit_set.dbt_typ, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Set Failure_is in Debit_look_account_status_ls to true
	    GO TO A110_GOT_DEBIT_END
	END-IF.

	If (Dsid_dupe_acc_ws NOT = SPACES )
*  Ambiguous.
	    Move SPACES to Dbt_account of Ent_debit_set
	    %Beg
	    Ent_debit_set.dbt_account CHANGE ;
	    Ent_debit_set.dbt_typ.dbt_ovr = "?" ;
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Not Str Thru: Found duplicate debit account ", 
			Dsid_know_acc_id, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    Set Failure_is in Debit_look_account_status_ls to true
	    GO TO A110_GOT_DEBIT_END
	END-IF.

A110_GOT_DEBIT_CLEANUP.

	Perform B320_SET_ACCOUNT through B320_SET_ACCOUNT_END.

	Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END.

%^
%^ At this point, we have a Channel and a good Account, check for the Bilat
%^
	Call "CUST_BILAT_ACCT".

A110_GOT_DEBIT_END.
    Exit.

B200_GET_NEWPARTY.
* Checks SI chain to see if we have a new credit party.
	Set Failure_is in Dsid_didansi_ws to TRUE
        Move 1 to Dsid_number_SIs
	Move SPACES to Dsid_previce_vstr
	%Beg  
	Dsid_SI_types = "DBTAIN" ;  
	Dsid_previce_vstr = NULL ;
	dsid_pr_ordinal_ws = <0>;
	%End.
	%^
	%^ 141229 
	If (Success_is in Dsid_got_channel_ws) and
	   ((clrhouse_is in Endpoint_type of Rchan_channel_set) Or
	    (RTGS_is in Endpoint_type of Rchan_channel_set))
	Then
	   %^ NO DBTAINS for RTGS systems
	    Move SPACES to Dsid_preferred_corr
	    %Beg  Dsid_preferred_corr = NULL ;  %End
	    Go to B200_GET_NEWPARTY_END
	End-if.
	%^
	%beg dsid_compose ^out(dsid_pr_type_ws) "DBTAIN",/;%end
	Call "PRULE_MSG_RULE_MATCH" Using
		By Reference dsid_pr_type_ws
		By Reference dsid_pr_type_ws_length
		by Reference dsid_pr_ordinal_ws
		by Reference dsid_pr_msglevel
		By Reference dsid_pr_source
		by reference dsid_pr_subtype_ws
		by reference dsid_pr_subtype_ws_length
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.


	If Success_is in Dsid_ret_stat And
 	  (dbt_idtype of dbt_typ of Ent_debit_set =
   	   "D" OR "G" OR "F" OR "V" ) 
	Then
		If ( NOT ( (Msg_ml in Dsid_pr_msglevel)
		        OR (Dbt_msg_ml in Dsid_pr_msglevel )
		        OR (Dbt_db_ml in Dsid_pr_msglevel) ) )
			%^ No AINS above address/message level
			Set Failure_is in dsid_ret_stat to True
		end-if
	end-if.



        If (Success_is in Dsid_ret_stat )
%^ Found a matching AIN SI.
	    Call "IS_PID_ADV_TYPE" Using
		By reference Bank of Loc_info of Ent_ftr_set
		By Reference Src_code of Ent_ftr_set
	      Returning Dsid_ret2_stat
	
            If (Src_code of Ent_ftr_set = "FED" OR "SEC")
	       OR (Success_is in Dsid_ret2_stat)
	       OR (Success_is in Dsid_210_ant_msg )
	    THEN
                %Beg
                Dsid_compose ^OUT(Dsid_info_memo)
                    "Debit AIN bypassed because source code is ",
                    Ent_ftr_set.src_code,/;
                %End
                Perform X920_INFO_MEMO through X920_INFO_MEMO_END
                Go to B200_GET_NEWPARTY_END
            END-IF


	    Perform GET_AIN_PARAMS thru
		    GET_AIN_PARAMS_END

	    %^ 83550 - was looping.
            If (idtype of dsid_preferred_corr =
		dbt_idtype of dbt_typ of ent_debit_set) Then
		    If  idtype of dsid_preferred_corr = "D" or "F" or "V" or "G"  Then
			%^ If our bank, strip for the comparision
			Move dbt_id_length of Ent_debit_set_lengths to dsid_tmp_fin_pos_ws
			If dbt_id of dbt_typ of ent_debit_set(dsid_tmp_fin_pos_ws:1) = "/"
			Then	%^ Discount the trailing / if present
					Subtract 1 from dbt_id_length of Ent_debit_set_lengths giving
					dsid_tmp_fin_pos_ws
			End-if 
			%^ First, if replacing with a larger key, don't bother checking
			%^ for dup
			%^ If our bank, strip for the comparision
			If idkey of dsid_preferred_corr(1:3) =  bnk_id of menu_bnk_union
			Then
				Move 5 to dsid_tmp_start_pos_ws
			Else
				Move 1 to dsid_tmp_start_pos_ws
			end-if
			Move idkey_length of dsid_preferred_corr_lengths to dsid_tmp_corr_len_ws
			If (idkey of dsid_preferred_corr(dsid_tmp_start_pos_ws:dsid_tmp_corr_len_ws) =
			     dbt_id of dbt_typ of
			     ent_debit_set(1:dsid_tmp_fin_pos_ws))
			Then
			    %^Check in length -1 due to addition of / at end of account number
			    %^ Bypass, we have a circular reference, replacing
			    %^ same with same.
			    Move SPACES to Dsid_preferred_corr
			    %Beg Dsid_preferred_corr = NULL ;  %End
			    Go to B200_GET_NEWPARTY_END
			end-if
		    Else
			If (idkey of dsid_preferred_corr  =
			    dbt_id of dbt_typ of ent_debit_set) Then
			    %^ Bypass, we have a circular reference, replacing
			    %^ same with same.
			    Move SPACES to Dsid_preferred_corr
			    %Beg  Dsid_preferred_corr = NULL ;  %End
			    Go to B200_GET_NEWPARTY_END
			end-if
		    end-if
	    end-if

	    Set Success_is in Dsid_didansi_ws to TRUE
%^ obselete	    %Beg  Dsid_preferred_corr = Ent_cnf_set.Si_party;  %End
            Evaluate dsid_pr_subtype_ws
	      When "SUB"
                 Set SUBSTITUTE in Dsid_pushdown to true

	      When "INS"
              When  SPACES
		 Set PUSHDOWN in Dsid_pushdown to true

	      When Other
		 %Beg
		 Dsid_compose ^OUT(Dsid_err_memo)
		           "Illegal submethod ",
			    dsid_pr_subtype_ws , " in SI ",
			    dsid_pr_ordinal_ws, " for ", 
				   Ent_debit_set.Dbt_typ, /;
		 %End
		 Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		 Set Failure_is in Debit_look_account_status_ls to true
                 Set Success_is in Dsid_push_error To TRUE
                 Set Success_is in Dsid_debit_acc_erred To TRUE
	    END-EVALUATE
	ELSE
	    Move SPACES to Dsid_preferred_corr
	    %Beg  Dsid_preferred_corr = NULL ;  %End
	END-IF.
B200_GET_NEWPARTY_END.

   EXIT.

%^ Temp - give it a number later, just here for now.
%^
GET_AIN_PARAMS.
	%^
	%^ Only party here now, can't think what else DBT AIN would need
	%^
	Move 0 to Lcl_prms_remaining_ws.
	Perform with test after until Failure_is in Lcl_scan_Stat_ws
		Move spaces to Lcl_Param_name_ws
		Call "PRULE_MSG_READEX_PARM" using
			by reference Lcl_Param_name_ws
			by reference Lcl_Param_name_ws_length
			by reference Lcl_Param_type_ws
			by reference Lcl_prms_remaining_ws
			by reference Lcl_Param_value_ws
			by reference Lcl_Param_value_ws_length
			returning Lcl_scan_stat_ws
		If Success_is in Lcl_scan_stat_ws then
			%^ All are alpha, copy use full fields
			Evaluate Lcl_Param_name_ws
		    		when "MTS$PARTY"
					%^ Parse the party ID/ should be Idtype/Id
					%beg Dsid_parse ^in(Lcl_param_value_ws)
		
						dsid_preferred_corr.Idtype , "/" ,
						dsid_preferred_corr.Idkey,/;
					%end
					%^ Parse should be enough but...
					If Not (Success_is in dsid_parse_status) Then
				    	    %^ lets jut move it in, maybe the / is missing
					    Move Lcl_param_value_ws(1:Lcl_param_value_ws_length) to
					         dsid_preferred_corr%^ should be format
					    %Beg
						dsid_preferred_corr( .Idtype change,
			    			  		     .Idkey change ) ;
					    %End
					end-if
			end-evaluate
		End-if
	End-perform.
%^
GET_AIN_PARAMS_END.
	EXIT.


B240_SET_ADDRESS.
* Sets actual rel address linkages in Message's debit party.
%^ Connect debit party address.
	%Beg
	BREAK: Ent_d_adr_set ;
	Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;  
	%End.

        Move Priority_flg of Ent_ftr_set to Dsid_hold_priority_ws.

	Call "SET_DEBIT_ADDRESS" USING
	    By reference Is_rptv_lookup_ls
	    By reference Nochange_bank_ls
	    By reference Dsid_loc_bank_change
	  RETURNING Dsid_ret_stat.

	If Dsid_loc_bank_change NOT = 0 
	    set success_is in Msg_bank_changed_ls to true
	END-IF	

	If (success_is in Msg_bank_changed_ls)
	   AND (Success_is in Nochange_bank_ls)
	THEN
%^ Write an error memo indicating the discrepancy.
	    %Beg
	    Dsid_compose ^OUT(Dsid_err_memo)
			"Debit party bank: ", Ent_d_adr_set.Bnk_id,
			" conflicts with message bank: "
			Ent_ftr_set.loc_info.bank, / ;
	    %End
	    Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	END-IF.

        If Priority_flg of Ent_ftr_set not = Dsid_hold_priority_ws
            %Beg
            Dsid_compose ^OUT(Dsid_info_memo)
            	"Priority set to ", Ent_ftr_set.flgs.priority_flg,
                " by debit party ",
		Ent_debit_set.dbt_typ(.dbt_idtype,"/",.dbt_id) , / ;

            %End
            Perform X920_INFO_MEMO thru X920_INFO_MEMO_END
        End-if.


* Completed address setup.
	%Beg
	BREAK: Dsid_acc_seq ;
	Ent_d_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
	%end
	Set Success_is in Dsid_set_address to TRUE.


B240_SET_ADDRESS_END.

   EXIT.

B270_PERFORM_FIND_ACC.

        Perform B280_FIND_ACCOUNT     through
                B280_FIND_ACCOUNT_END. 

%^ SPR 43062 see if there's an equivalent currency
%^           if so, try to get account in that currency.
        If (Dsid_know_acc_id = SPACES)
         then
          Move SPACES to Equiv_curr_ws
          Call "GET_EQUIV_CURRENCY" USING
             By reference Dsid_currency_ws
             by reference Equiv_curr_ws
                RETURNING Equiv_ret_status
          If Success_is in Equiv_ret_status 
           then
            Move Dsid_currency_ws to Save_dsid_curr_ws
            Move Equiv_curr_ws to Dsid_currency_ws
            Perform B280_FIND_ACCOUNT      through
                    B280_FIND_ACCOUNT_END
            If (Dsid_know_acc_id = SPACES )
             then
              Move Save_dsid_curr_ws to Dsid_currency_ws
            end-if
          end-if
        end-if.

B270_PERFORM_FIND_ACC_END.

   EXIT.

B280_FIND_ACCOUNT.
%^  Uses following algorithm to choose an account from our address:
%^ 	If the message source is a clearinghouse (identifier "P")
%^		If the address was found by using an account id for that
%^			clearinghouse, keep it as the id
%^		Else
%^		    If the address is a null-suffix Pid and we are using
%^			suffixes
%^			Just append the proper suffix
%^		    Else
%^			The address has the wrong suffix so flag the error.
%^		    End-if
%^		End-if
%^	ELSE
%^	If address was found by using an account identifier ("D" or "G" or "F"
%^		or "V")                              
%^	    try the account which matches the identifier
%^	    If we find the account AND its currency matches
%^		    Check for another account with same id and currency.
%^	    ELSE
%^		Fall through to search by currency below.
%^	    END-IF
%^	ELSE
%^	    look for the default account.
%^	    If we find it and its currency matches
%^		We are done, since the default cannot be ambiguous.
%^	    ELSE
%^		Fall through to search by currency below.
%^	    END-IF
%^	END-IF
%^ Search by currency:
%^	Step through the accounts for the address looking for a currency match.
%^	If we find it
%^	    Remember the account.
%^	    Continue searching for a second match (ambiguity detection).
%^	End-if
%^
%^ We return the account we found and the first ambiguous account.
%^ If the Dsid_ambig_ws argument is "T", we will suppress ambiguity
%^ checking.
%^
	%^ Check to see if this a "P" equivalent advice type
%^OBSELETE        Call "GET_PID_ADV_DATA" using

	%Beg
	        Dsid_acc_bank_ws = Menu_bnk_union.bnk_id ;
       	        Dsid_bank_curr_ws = Menu_bnk_union.Base_currency_id ;
        	BREAK: Dsid_acc_seq ;
		BREAK: Ent_d_acc_set ;
		BREAK: Ent_d_acc_rel_reg;
        	Ent_d_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
        %End.

	Perform B300_GET_ACC_FROM_CHAN thru
	        B300_GET_ACC_FROM_CHAN_END.


%^ If we already know the account id, we can just exit
        If (Dsid_know_acc_id NOT = SPACES)
	   AND (Dsid_dupe_acc_ws = SPACES )
	THEN
	    %ace_conn_root_q Rel_acc_index ;
	    %Beg
	    SEARCH: Rel_acc_index (FORWARD, GEQ, .Rel_name_key 
				(.Idbank        = Dsid_know_acc_id.Idbank,
				 .Idtype        = Dsid_know_acc_id.Idtype,
				 .Idkey (.Idacc = Dsid_know_acc_id.Idkey,
				         .Idadr = NULL,
					 .Idpad = NULL ) ) ) ;
            %End
	    If (Success_is in Rel_acc_index_status   )
	       AND (Idbank of Rel_name_key of Rel_acc_index =
						Idbank of Dsid_know_acc_id )
	       AND (Idtype of Rel_name_key of Rel_acc_index =
						Idtype of Dsid_know_acc_id )
	       AND (Idacc of Idkey of Rel_name_key of Rel_acc_index =
						Idkey of Dsid_know_acc_id )
	    THEN
		%Beg  
		Rel_acc_index CONN: Ent_d_acc_set(
				REG:  Ent_d_acc_rel_reg (NOMOD));  
		%End
	        If (Dsid_currency_ws = SPACES OR Currency_id of Ent_d_acc_set )
	            GO TO B280_FIND_ACCOUNT_END
		END-IF
                If (Dsid_currency_ws = Base_currency_id of Menu_bnk_union and
                    Currency_id of Ent_d_acc_set = spaces)
                    GO TO B280_FIND_ACCOUNT_END
                END-IF
		%Beg  
		BREAK: Ent_d_acc_set ;
		BREAK: Ent_d_acc_rel_reg;
	        Dsid_compose ^OUT(Dsid_err_memo)
		    "Cannot connect to debit account ", Dsid_know_acc_id, / ;
	        %End
	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	        %Beg  	Rel_acc_index (EQL) ;  %End
                GO TO B280_FIND_ACCOUNT_END
	    END-IF
	    %Beg  Ent_debit_set.Dbt_account = Dsid_know_acc_id ;  %End
            GO TO B280_FIND_ACCOUNT_END
	END-IF.
        %Beg
	Dsid_know_acc_id = NULL ;
        Dsid_dupe_acc_ws = NULL ;
        FIRST: Dsid_acc_seq ;
        %End

	If (Failure_is in Dsid_acc_seq_status   ) then
%^ We have no accounts for this address.             
	    %Beg  BREAK: Dsid_acc_seq ;  %End
	    If (Success_is in Is_payment_ls)
	        %Beg
                Dsid_compose ^OUT(Dsid_err_memo)
		    "Not Str Thru: Debit party has no accounts.", / ;
		%End
	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	    END-IF
	    GO TO B280_FIND_ACCOUNT_END
	END-IF.

%^ No account type restriction.  Let's see if an account type was used
%^    to get the address.  If so, then let's try it.
	Move SPACE to Dsid_idtype_ws.
	If (Dbt_idtype of Dbt_typ of Ent_debit_set = "D" OR "G" OR "F" OR "V" )
	    Move Dbt_idtype of Dbt_typ of Ent_debit_set to Dsid_idtype_ws
	END-IF.

	%^ Spr 95707
	If Account_type_ls Not = Spaces Then
		%^ we were givin an override, use it
		Move Account_type_ls to Dsid_idtype_ws
	end-if.

* If this is an incoming CHIPS or equivalent, but the debit account has already
*  been identified by account number as a non-"P" idtype, then retain that 
* account number. Otherwise insist on an account idtype of "P"
%^ NO MORE P ACCOUNT TYPE
%^	IF Success_is in Dsid_itsa_clearhouse
%^ #58253 & #59778 do not force Dsid_idtype_ws to "P" if this is a Swift 210 
%^	           anticipation
%^	THEN
%^	    If Dsid_idtype_ws NOT = "D" AND "G" AND "F" AND "V"
%^		Move "P" to Dsid_idtype_ws
%^	    End-if
%^      END-IF.

	Set Failure_is in Dsid_found_it to TRUE
	If ((Dbt_idtype of Dbt_typ of Ent_debit_set = Dsid_idtype_ws )
	    AND (Dsid_idtype_ws NOT = SPACE )) Or
	    (Account_type_ls NOT = Spaces)
	THEN
		If ((Dbt_idtype of Dbt_typ of Ent_debit_set = Dsid_idtype_ws )
		     and dsid_idtype_ws NOT = Space)
	 	   %Beg
            		Dsid_parse ^IN(Ent_debit_set.dbt_typ.dbt_id),
                		^OPTION(^STRING<3>,":"),
				Dsid_acc_id_ws,
                		^ONEOF("/",(^SPACE,/));
	    	   %End
		Else
		   %^ We're here via a requested ID type, blank to ID
		   %beg dsid_acc_id_ws = Null; %end
		end-if

	    If ((Success_is in Dsid_parse_status) And
	      	(Dsid_acc_id_ws_length NOT = 0 )  And
	      	(Dsid_acc_id_ws NOT = SPACES )) Or
		(Account_type_ls NOT = Spaces) %^ Use a Null id, and get a requested typ in Curr
	    THEN

                %Beg	
	        SEARCH: Dsid_acc_seq ( FORWARD, GEQ, .Rel_name_key
			    ( .Idbank = Ent_debit_set.dbt_adr_bnk_id,
	    		      .Idtype = Dsid_idtype_ws,
                              .Idkey( .Idacc = Dsid_acc_id_ws,
				      .Idadr = NULL,
				      .Idpad = NULL ) ) ) ;
                %End
	        Set Failure_is in Dsid_curr_okay to TRUE
	        Set Failure_is in Dsid_found_account to true
	        PERFORM UNTIL (Failure_is in Dsid_acc_seq_status   )
			      OR (Success_is in Dsid_found_account )
		    If (Success_is in Dsid_acc_seq_status   )
		        AND (Idbank of Rel_name_key of Dsid_acc_seq =
					Dbt_adr_bnk_id of Ent_debit_set )
		        AND (Idtype of Rel_name_key of Dsid_acc_seq =
					Dsid_idtype_ws )
		        AND ( (Dsid_acc_id_ws_length = 0 )
			    OR (Idacc of Idkey of Rel_name_key of Dsid_acc_seq
				 = Dsid_acc_id_ws(1:Dsid_acc_id_ws_length) ) ) 
		    THEN       
		        If (Disp_nof of Dsid_acc_seq NOT = SPACE) 
			  AND (Rtn_dflt_acct of Menu_cfg = "Y") 
		        THEN
                            %Beg
 	    		    Dsid_acc_seq EQUATE: Dsid_accdef_seq(NOMOD) ;
			    SCAN: Dsid_accdef_seq (NEQ, Key = " " ) ;
			    %End

                            If Success_is in Dsid_accdef_seq_status   then
                                %beg
			        BREAK: Dsid_acc_seq;
			        Dsid_accdef_seq EQUATE: Dsid_acc_seq(NOMOD);
			        %End
		                Perform C480_CHECK_CURRENCY through
							C480_CHECK_CURRENCY_END
	    		        If (Success_is in Dsid_curr_okay )
				    Set Success_is in Dsid_found_account to TRUE
				    %beg Break: dsid_accdef_seq; %end
			        ELSE
			            %Beg
	        	            Dsid_compose ^OUT(Dsid_err_memo)
				    	"Default account ", 
				     	Dsid_acc_seq .rel_name_key.idtype,
				         	 "/" ,
				    	Dsid_acc_seq .rel_name_key.idkey.idacc,
				    	" has an incompatible currency",/;
	        	            %End
	                            Perform X900_ERROR_MEMO through 
				   	    		    X900_ERROR_MEMO_END
		    		    Set Failure_is in
					   Debit_look_account_status_ls to true
                                    %Beg BREAK: Dsid_accdef_seq; %End
		    	            GO TO B280_FIND_ACCOUNT_END
			        END-IF
			    ELSE
				%Beg
	        	        Dsid_compose ^OUT(Dsid_err_memo)
				   "No default account assigned to replace ", 
				       Dsid_acc_seq .rel_name_key.idtype, 
				       "/" ,
				     Dsid_acc_seq .rel_name_key.idkey.idacc,/;
	        	        %End
	                        Perform X900_ERROR_MEMO through 
							     X900_ERROR_MEMO_END
		    	        Set Failure_is in
					   Debit_look_account_status_ls to true
                                %Beg BREAK: Dsid_accdef_seq; %End
		    	        GO TO B280_FIND_ACCOUNT_END
			    END-IF
		        END-IF
			If (Failure_is in Dsid_found_account )
			    If (Dsid_acc_id_ws_length NOT = 0 ) Or
			       (Idtype of Rel_name_key of Dsid_acc_seq = Dsid_idtype_ws) 
		       		Perform C480_CHECK_CURRENCY through 
							C480_CHECK_CURRENCY_END
				If (Success_is in Dsid_curr_okay)
				    Set Success_is in Dsid_found_account to TRUE
				END-IF
			    ELSE
				Set Failure_is in Dsid_found_account to TRUE
			    END-IF
			END-IF
		    END-IF
		    If (Failure_is in Dsid_found_account )
		       %Beg  NEXT: Dsid_acc_seq ;  %End
		    END-IF
	        END-PERFORM
	    END-IF

*  Should now have either matching account in correct currency or a miss.
	    If (Success_is in Dsid_found_account)
*  It's a hit.
		%Beg
		Dsid_know_acc_id.Idtype = 
				Dsid_acc_seq.rel_name_key.idtype ;
		Dsid_know_acc_id.Idbank = 
				Dsid_acc_seq.rel_name_key.idbank ;
		Dsid_know_acc_id.Idkey = 
				Dsid_acc_seq.rel_name_key.idkey.idacc ;
		%End
		If (Dsid_ambig_ws NOT = "T" )
		    Perform C520_CHECK_DUPE through C520_CHECK_DUPE_END
		END-IF
	    	GO TO B280_FIND_ACCOUNT_END
	    END-IF
	END-IF.

%^ If source is via a clearinghouse
	%^ LOGIC REMOVEDIF Success_is in Dsid_itsa_clearhouse
	%^ P as an account type is OBSELETE, Remove the logic.
	%^		*  Search the account sequence for accounts of the specified type.
	%^
	%^	END-IF.

* No account info specified -- just try for the default.
	    %Beg
	    Ent_d_adr_set.account_seq CONN: Dsid_accdef_seq(NOMOD);
	    SCAN: Dsid_accdef_seq(NEQ, Key = " " ) ;
	    %End
	    If (Disp_default of Dsid_accdef_seq NOT = SPACES )
              AND ( (Success_is in Dsid_itsa_clearhouse) 
               	OR (Success_is in Dsid_210_ant_msg)
                OR (Idtype of Rel_name_key of Dsid_accdef_seq NOT = "P" ) )
            THEN
* There was a default account.
		%Beg
		BREAK: Dsid_acc_seq;
		Dsid_accdef_seq EQUATE: Dsid_acc_seq(NOMOD);
		BREAK: Dsid_accdef_seq;
		%End
	        Perform C480_CHECK_CURRENCY through C480_CHECK_CURRENCY_END
	        If (Success_is in Dsid_curr_okay )
* It matches!
		    %Beg
		    Dsid_know_acc_id.Idtype = 
				Dsid_accdef_seq.rel_name_key.idtype ;
		    Dsid_know_acc_id.Idbank = 
				Dsid_accdef_seq.rel_name_key.idbank ;
		    Dsid_know_acc_id.Idkey = 
				Dsid_accdef_seq.rel_name_key.idkey.idacc ;
		    %End
		    GO TO B280_FIND_ACCOUNT_END
	        END-IF
	    ELSE
	        %Beg  BREAK: Dsid_accdef_seq;  %End
	    END-IF.

%^
%^ New step, try for a DDA before looking for a pure currency match, unless
%^ a specific Account_type was passed in, where we should look up that specific
%^ account type/currency.
%^
        %Beg
        BREAK: Dsid_acc_seq ;
        Ent_d_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
        FIRST: Dsid_acc_seq ;
        %End.

	If Account_type_ls = SPACES then
	    Move "D" to Dsid_idtype_ws		%^ try for a D unless override passed in
	Else
	    Move Dsid_idtype_ws to Dsid_hold_idtype_ws
	End-if.


	PERFORM C560_FIND_ACC_CUR through C560_FIND_ACC_CUR_END.
	If (Success_is in Dsid_curr_okay AND Success_is in Dsid_type_okay)
* We found one account.  Remember it.
	    %Beg
	    Dsid_know_acc_id.Idtype = 
				Dsid_acc_seq.rel_name_key.idtype ;
	    Dsid_know_acc_id.Idbank = 
				Dsid_acc_seq.rel_name_key.idbank ;
	    Dsid_know_acc_id.Idkey = 
				Dsid_acc_seq.rel_name_key.idkey.idacc ;
	    %End
	    If (Dsid_ambig_ws NOT = "T" )
		%Beg  NEXT: Dsid_acc_seq;  %End
	        If (Success_is in Dsid_itsa_clearhouse)
                   OR (Success_is in Dsid_210_ant_msg)
	        THEN
		    Perform UNTIL (Failure_is in Dsid_acc_seq_status )
		             OR (Idtype of Rel_name_key of Dsid_acc_seq = "P")
		        %Beg  NEXT: Dsid_acc_seq;  %End
		    END-PERFORM
	        ELSE
	            Perform UNTIL (Failure_is in Dsid_acc_seq_status   )
			     	OR (Idtype of Rel_name_key 
					     of Dsid_acc_seq NOT = "P" AND "Y" AND "L")
		        %Beg  NEXT: Dsid_acc_seq;  %End
	            END-PERFORM
	        END-IF
	        If (Success_is in Dsid_acc_seq_status   )
		   AND ( ( (Failure_is in Xbank_account_ok_ls )
                           AND ( Dbt_adr_bnk_id of Ent_debit_set  =
			      Idbank of Rel_name_key of Dsid_acc_seq ) )
			 OR (Success_is in Xbank_account_ok_ls ) )
		THEN
               	    PERFORM C560_FIND_ACC_CUR through C560_FIND_ACC_CUR_END
		    If (Success_is in Dsid_curr_okay AND Success_is in Dsid_type_okay)
* Alas, we have a second currency hit.
			%Beg
			Dsid_dupe_acc_ws.Idtype = 
					Dsid_acc_seq.rel_name_key.idtype ;
	    		Dsid_dupe_acc_ws.Idbank = 
					Dsid_acc_seq.rel_name_key.idbank ;
	    		Dsid_dupe_acc_ws.Idkey =  
					Dsid_acc_seq.rel_name_key.idkey.idacc ;
	    		%End
		    END-IF
		END-IF
	    END-IF
	    GO TO B280_FIND_ACCOUNT_END
	END-IF.

%^ Need to find a specific type if that's that was asked for.

	If Account_type_ls not = SPACE then
	    Set Failure_is in Dsid_found_it to TRUE
	    GO TO B280_FIND_ACCOUNT_END
	End-if.

	Move Dsid_hold_idtype_ws to Dsid_idtype_ws. 	%^ rest what we set back to what it was

* If we get here, we couldn't find a match on an account id (probably due to
*    currency mismatch) or we currency-mismatched with the default or there
*    was no default.  In any case, we would now like to try for a currency 
*    match.
        %Beg
        BREAK: Dsid_acc_seq ;
        Ent_d_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
        FIRST: Dsid_acc_seq ;
        %End.
	If (Failure_is in Xbank_account_ok_ls )
* Position ourselves into the correct bank -- it's the top of the key
* However we must skip over wrong types.
	    If (Success_is in Dsid_itsa_clearhouse)
               OR (Success_is in Dsid_210_ant_msg)
	    THEN
		Perform UNTIL (Failure_is in Dsid_acc_seq_status )
		       OR (Idtype of Rel_name_key of Dsid_acc_seq = "P")
		    %Beg  NEXT: Dsid_acc_seq;  %End
		END-PERFORM
	    ELSE
	        Perform UNTIL ( (Failure_is in Dsid_acc_seq_status   )
			     OR (Idtype of Rel_name_key 
					   of Dsid_acc_seq NOT = "P" AND "Y" AND "L") )
		    %Beg  NEXT: Dsid_acc_seq;  %End
	        END-PERFORM
	    END-IF
	    Perform UNTIL (Idbank of Rel_name_key of Dsid_acc_seq =
					Dbt_adr_bnk_id of Ent_debit_set )
			  OR (Failure_is in Dsid_acc_seq_status   )
	        If (Success_is in Dsid_itsa_clearhouse)
                   OR (Success_is in Dsid_210_ant_msg)
	        THEN
		    Perform UNTIL (Failure_is in Dsid_acc_seq_status )
		       	     OR (Idtype of Rel_name_key of Dsid_acc_seq = "P")
		        %Beg  NEXT: Dsid_acc_seq;  %End
		    END-PERFORM
	        ELSE
	            Perform UNTIL (Failure_is in Dsid_acc_seq_status   )
			     	OR (Idtype of Rel_name_key 
					    of Dsid_acc_seq NOT = "P" AND "Y" AND "L")
		        %Beg  NEXT: Dsid_acc_seq;  %End
	            END-PERFORM
	        END-IF
	    END-PERFORM
	END-IF.
	PERFORM C560_FIND_ACC_CUR through C560_FIND_ACC_CUR_END.
	If (Success_is in Dsid_curr_okay AND Success_is in Dsid_type_okay)
* We found one account.  Remember it.
	    %Beg
	    Dsid_know_acc_id.Idtype = 
				Dsid_acc_seq.rel_name_key.idtype ;
	    Dsid_know_acc_id.Idbank = 
				Dsid_acc_seq.rel_name_key.idbank ;
	    Dsid_know_acc_id.Idkey = 
				Dsid_acc_seq.rel_name_key.idkey.idacc ;
	    %End
	    If (Dsid_ambig_ws NOT = "T" )
		%Beg  NEXT: Dsid_acc_seq;  %End
	        If (Success_is in Dsid_itsa_clearhouse)
                   OR (Success_is in Dsid_210_ant_msg)
	        THEN
		    Perform UNTIL (Failure_is in Dsid_acc_seq_status )
		             OR (Idtype of Rel_name_key of Dsid_acc_seq = "P")
		        %Beg  NEXT: Dsid_acc_seq;  %End
		    END-PERFORM
	        ELSE
	            Perform UNTIL (Failure_is in Dsid_acc_seq_status   )
			     	OR (Idtype of Rel_name_key 
					     of Dsid_acc_seq NOT = "P" AND "Y" AND "L")
		        %Beg  NEXT: Dsid_acc_seq;  %End
	            END-PERFORM
	        END-IF
	        If (Success_is in Dsid_acc_seq_status   )
		   AND ( ( (Failure_is in Xbank_account_ok_ls )
                           AND ( Dbt_adr_bnk_id of Ent_debit_set  =
			      Idbank of Rel_name_key of Dsid_acc_seq ) )
			 OR (Success_is in Xbank_account_ok_ls ) )
		THEN
               	    PERFORM C560_FIND_ACC_CUR through C560_FIND_ACC_CUR_END
		    If (Success_is in Dsid_curr_okay AND Success_is in Dsid_type_okay)
* Alas, we have a second currency hit.
			%Beg
			Dsid_dupe_acc_ws.Idtype = 
					Dsid_acc_seq.rel_name_key.idtype ;
	    		Dsid_dupe_acc_ws.Idbank = 
					Dsid_acc_seq.rel_name_key.idbank ;
	    		Dsid_dupe_acc_ws.Idkey =  
					Dsid_acc_seq.rel_name_key.idkey.idacc ;
	    		%End
		    END-IF
		END-IF
	    END-IF
	END-IF.
B280_FIND_ACCOUNT_END.
   EXIT.

B300_GET_ACC_FROM_CHAN.
%^
%^	Load the Channel Account if we can
%^
%^
	Set ID_IS in dsid_prchan_mode to True.
	If Dbt_adr_bnk_id of Ent_debit_set not = spaces then
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_debit_set.dbt_adr_bnk_id;
		%End
	Else
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_ftr_set.Loc_info.Bank;
		%End
	End-if.

	%Beg
		dsid_chan_ident_ws.idkey = Ent_ftr_set.src_code;
	%End

	Call "GET_CHANNEL" using
			By Reference 	dsid_prchan_mode
			By Reference	dsid_chan_ident_ws
			By Reference 	dsid_chan_ident_ws_lengths
		returning dsid_got_channel_ws.

	If (Success_is in Dsid_got_channel_ws) and
	   ((clrhouse_is in Endpoint_type of Rchan_channel_set) Or
	    (RTGS_is in Endpoint_type of Rchan_channel_set))
	Then
		%^ If we have an 'on us' idtype, don't override with
		%^ network Account most likely a drawdown
		%^
		If  Dbt_idtype of dbt_typ of  ent_debit_set
			Not = "D" AND "G" AND "F" AND "V" Then    
		   %^ See where the account number should come from
		   If Endpoint_account of rchan_channel_set = Spaces Then
			Call "PRULE_SET_CHANNEL" using
				By Reference	dsid_chan_ident_ws
				By Reference 	dsid_chan_ident_ws_lengths
			returning dsid_pr_ret_stat
			%beg dsid_prm_values_remain = <0>; %end
			Call "PRULE_CHAN_GET_PARAM" using
				by reference Mts_channel_account_wc
				by reference Mts_channel_account_wc_length
				by reference dsid_prm_edit_type
				by reference dsid_prm_values_remain
				by reference dsid_prm_value
				by reference dsid_prm_value_length
			returning dsid_pr_ret_stat
			If Success_is in dsid_pr_ret_stat then
				%Beg
					dsid_parse ^in(dsid_prm_value)
			       			dsid_know_acc_id.Idtype, "/",
			       		^Oneof( (dsid_know_acc_id.Idbank, ":", dsid_know_acc_id.IdKey, /),
				       		(dsid_know_acc_id.IdKey, /) );
				%End
				If Success_is in dsid_parse_status then
					Move idkey of dsid_know_acc_id to debit_account_ls
				End-if
		        End-if
			Call "PRULE_BREAK_CHANNEL" returning dsid_pr_ret_stat
		    else
			%^ Channel Determination provided an account number use, it
			%beg
			   dsid_know_acc_id = rchan_channel_set.endpoint_account;
			%end
			Move Idkey of Dsid_know_acc_id to Debit_account_ls
		end-if
	end-if.

B300_GET_ACC_FROM_CHAN_END.

	EXIT.

B320_SET_ACCOUNT.
%^ Finds actual debit account set, hooks it up, and copies necessary info.
	%Beg  Dsid_conn_ws = Ent_d_acc_set State.conn ;  %End
	If (Dsid_conn_ws = 0 )
	    %ace_conn_root_q Rel_acc_index ;
	    %Beg
	    SEARCH: Rel_acc_index (FORWARD, GEQ, .Rel_name_key 
				(.Idbank        = Dsid_know_acc_id.Idbank,
				 .Idtype        = Dsid_know_acc_id.Idtype,
				 .Idkey (.Idacc = Dsid_know_acc_id.Idkey,
				         .Idadr = NULL,
			       		 .Idpad = NULL ) ) ) ;
            %End
	    If (Failure_is in Rel_acc_index_status   )
	       OR (Idbank of Rel_name_key of Rel_acc_index NOT =
						Idbank of Dsid_know_acc_id )
	       OR (Idtype of Rel_name_key of Rel_acc_index NOT =
						Idtype of Dsid_know_acc_id )
	       OR (Idacc of Idkey of Rel_name_key of Rel_acc_index NOT =
						Idkey of Dsid_know_acc_id )
	    THEN
	        %Beg
	        Dsid_compose ^OUT(Dsid_err_memo)
		    "Cannot connect to debit account ", Dsid_know_acc_id, / ;
	        %End
	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
	        %Beg  	Rel_acc_index (EQL) ;  %End
                GO TO B320_SET_ACCOUNT_END
	    END-IF
	    %Beg  
  	    Rel_acc_index CONN: Ent_d_acc_set(NOMOD,
				REG:  Ent_d_acc_rel_reg (NOMOD));  
	    %End
	    If (Failure_is in Rel_acc_index_status   )
		OR (Failure_is in Ent_d_acc_set_status   )
	    THEN
	        %Beg
	        Dsid_compose ^OUT(Dsid_err_memo)
			"Cannot connect to debit account ", 
			Dsid_know_acc_id, / ;
	        %End
	        Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		%Beg  	Rel_acc_index (EQL) ;  %End
                GO TO B320_SET_ACCOUNT_END
	    END-IF
        END-IF.
	Call "SET_DEBIT_ACCOUNT" USING
	    By reference Debit_account_ls         
	    By reference Is_rptv_lookup_ls
	    By reference Special_fee_key_ls
	  RETURNING Dsid_ret_stat  
* Completed account setup.
	Set Success_is in Dsid_set_account to TRUE
	Move Currency_id of Ent_d_acc_set to Currency_found_ls
	If (Currency_found_ls = SPACES)
	THEN
	    %Beg
	    BREAK: Dsid_bnk_union ;
	    SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
	    %End
	    If Failure_is in Bnk_index_status  
		Move SPACES to Currency_found_ls
	    ELSE
		%Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		Move Base_currency_id of Dsid_bnk_union to Currency_found_ls
	    END-IF
	END-IF.
	%Beg  	Rel_acc_index (EQL) ;  %End.

B320_SET_ACCOUNT_END.

   EXIT.

%^ the following in never performed so comment it out - csh.
%^C400_FIND_ADR_CUR.
%^*
%^*  We have an ambiguous debit party address.
%^*  We also have a non-blank currency which we can use to resolve the
%^*  ambiguity.  We will scan through all of the addresses and check
%^*  the currencies on their associated accounts.  If we find only one
%^*  address with a matching currency, we have successfully disambiguated.
%^%^*  If we are in "nocheck_ambig" mode, we will return the first address
%^*  with an account in the correct currency.
%^        
%^	%ace_conn_root_q Rel_index ;
%^	Set Success_is in Dsid_next_status to true.
%^	Move ZERO to Dsid_qualified_rel.
%^	PERFORM WITH TEST BEFORE UNTIL (Failure_is in Dsid_next_status   )
%^	    Move Rel_id of Relget_adr_set to Dsid_rel_id
%^	    %Beg
%^	    BREAK: Dsid_adr_set ;
%^	    BREAK: Dsid_acc_seq ;
%^	    Rel_index ^SEARCH (Key = Dsid_rel_id);
%^	    %End
%^	    If (Success_is in Rel_index_status   )
%^		AND (OBJECT_IS in Rel_index_cursor )
%^		AND (ADDRESS_IS in Rel_type of Rel_index)
%^	    THEN
%^		%Beg
%^		Rel_index CONN: Dsid_adr_set(NOMOD) ;
%^		Dsid_adr_set.account_seq CONN: 
%^					Dsid_acc_seq ^FIRST (NOMOD) ;
%^                %End
%^
%^		If (Xbank_account_ok_ls = 0 )
%^* Position ourselves into the correct bank -- it's the top of the key
%^	    	    Perform UNTIL 
%^			(Idbank of Rel_name_key of Dsid_acc_seq =
%^					Dbt_adr_bnk_id of Ent_debit_set )
%^			  OR (Failure_is in Dsid_acc_seq_status   )
%^			%Beg  NEXT: Dsid_acc_seq ;  %End
%^	    	    END-PERFORM
%^		END-IF
%^		PERFORM with TEST BEFORE UNTIL
%^				(Failure_is in Dsid_acc_seq_status   )
%^		                OR ( (Xbank_account_ok_ls = 0 )
%^                                    AND ( Bnk_id of Dsid_adr_set NOT =
%^			      Idbank of Rel_name_key of Dsid_acc_seq ) )
%^*  Truck through the account sequence looking for a matching currency.
%^		    PERFORM C480_CHECK_CURRENCY through C480_CHECK_CURRENCY_END
%^		    If (Success_is in Dsid_curr_okay)
%^*  Found account with matching currency.
%^			If (Dsid_qualified_rel = 0 )
%^			    Move Rel_id of Dsid_adr_set to Dsid_qualified_rel
%^			    If (Dsid_ambig_ws = "T" )
%^* First hit is acceptable, so we are done.
%^				GO TO C400_FIND_ADR_CUR_EXIT
%^			    END-IF
%^			ELSE
%^* We already have a hit.  Shucky darns, we are still ambiguous.
%^			    Move ZERO to Dsid_qualified_rel
%^			    GO TO C400_FIND_ADR_CUR_EXIT
%^			END-IF
%^		    ELSE
%^                        %Beg  NEXT: Dsid_acc_seq ;  %End
%^		    END-IF
%^		END-PERFORM
%^            END-IF
%^	    Call "NEXT_ACCT_LOOKUP" using
%^	        by reference Dbt_idtype of Dbt_typ of Ent_debit_set
%^	        by reference Dbt_id of Dbt_typ of Ent_debit_set
%^	        by reference Dsid_ovr
%^	      RETURNING Dsid_next_status
%^        END-PERFORM.
%^C400_FIND_ADR_CUR_EXIT.
%^	%Beg
%^	BREAK: Dsid_adr_set ;
%^	BREAK: Dsid_acc_seq ;
%^        %End.
%^C400_FIND_ADR_CUR_END.
%^

%^   EXIT.
C440_COPY_ADDRESS.
%^
	If (Adr_name_length of Relget_adr_set_lengths = ZERO )
	   AND (Dbt_name1_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name1 = NULL;  %End
	END-IF.
	If (Adr1_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name2_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name2 = NULL;  %End
	END-IF.
	If (Adr2_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name3_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name3 = NULL;  %End
	END-IF.
	If (Adr3_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name4_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name4 = NULL;  %End
	END-IF.
%^
	%Beg
	Dsid_Compose Relget_adr_set (
		.Adr_name (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name1), ^_, /)),
		.Adr1 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name2), ^_, /)),
		.Adr2 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name3), ^_, /)),
		.Adr3 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name4), ^_, /))) ;
%^ Build a DBT_ADR_TYPE field from country code and 
%^   ADR_TYPE field of Relget_adr_set
	Dsid_Compose ^OUT(Ent_debit_set.Dbt_adr_type),
		Relget_adr_set.Country_code(^STRING<2>),
		Relget_adr_set.Adr_type, /;
	%End.

%^ copy in the zip code too
	If (Zip of Relget_adr_set NOT = SPACES )
	    Call "ZIPSUB" Using
		by reference Dbt_name4 of Ent_debit_set
		by reference Dbt_name4_length of Ent_debit_set_lengths
		by reference Dbt_name3 of Ent_debit_set
		by reference Dbt_name3_length of Ent_debit_set_lengths
		by reference Zip of Relget_adr_set
		by reference Line_flg_ws

	    EVALUATE Line_flg_ws
		WHEN "4"
		    %Beg  Ent_debit_set.Dbt_name4 CHANGE;  %End

		WHEN "3"
		    %Beg  Ent_debit_set.Dbt_name3 CHANGE;  %End

	    END-EVALUATE
	END-IF.
C440_COPY_ADDRESS_END.


   EXIT.
C480_CHECK_CURRENCY.
* Checks currency of account in Dsid_acc_seq against message.  If they 
* match, Success_is is in Dsid_curr_okay.  Else Failure_is is in it.

	If (Dsid_currency_ws = SPACES )
* Message currency is unknown /don't care.
	    Set Success_is in  Dsid_curr_okay to TRUE
	    GO TO C480_CHECK_CURRENCY_END
	END-IF.

	If (Idbank of Rel_name_key of Dsid_acc_seq = Bnk_id of Menu_bnk_union )
	    Move Base_currency_id of Menu_bnk_union to Dsid_acc_curr_ws
	ELSE
	    If (Idbank of Rel_name_key of Dsid_acc_seq = Dsid_acc_bank_ws )
%^ Note caching here to try to cut down on lookups.
		Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
	    ELSE
%^  Rats.  We have to look up the bank.
		%Beg
		BREAK: Dsid_bnk_union ;
		SEARCH: Bnk_index 
			(Key = Dsid_acc_seq .Rel_name_key.Idbank ) ;
		%End
		If Failure_is in Bnk_index_status  
		    Move SPACES to Dsid_acc_curr_ws
		ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Bnk_id of Dsid_bnk_union to Dsid_acc_bank_ws
		    Move Base_currency_id of Dsid_bnk_union to 
			Dsid_bank_curr_ws
		    Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
		END-IF
	    END-IF
	END-IF.
	If (Disp_cur of Dsid_acc_seq NOT = SPACES )
	    Move Disp_cur of Dsid_acc_seq to Dsid_acc_curr_ws
	END-IF.	
        If (Dsid_acc_curr_ws = Dsid_currency_ws )
	    Set Success_is in Dsid_curr_okay to TRUE
	ELSE
	    Set Failure_is in Dsid_curr_okay to TRUE
	END-IF.
C480_CHECK_CURRENCY_END.

   EXIT.
%^C500_CHECK_TYP_CURRENCY.
* Checks currency of account in Dsid_acctyp_seq against message.  If they 
* match, Success_is returned in Dsid_curr_okay.  Else Failure_is is returned.
%^        If (Dsid_currency_ws = SPACES )
* Message currency is unknown /don't care.
%^	    Set Success_is in Dsid_curr_okay to TRUE
%^	    GO TO C500_CHECK_TYP_CURRENCY_END
%^	END-IF.
%^
%^	If (Idbank of Rel_name_key of Dsid_acctyp_seq =
%^						     Bnk_ID of Menu_bnk_union )
%^	    Move Base_currency_id of Menu_bnk_union to Dsid_acc_curr_ws
%^	ELSE
%^	    If (Idbank of Rel_name_key of Dsid_acctyp_seq = 
%^							Dsid_acc_bank_ws )
%^ Note caching here to try to cut down on lookups.
%^		Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
%^	    ELSE
%^  Rats.  We have to look up the bank.
%^		%Beg
%^		BREAK: Dsid_bnk_union ;
%^		SEARCH: Bnk_index 
%^			(Key = Dsid_acctyp_seq.Rel_name_key.Idbank ) ;
%^		%End
%^		If Failure_is in Bnk_index_status  
%^		    Move SPACES to Dsid_acc_curr_ws
%^		ELSE
%^		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
%^		    Move Bnk_id of Dsid_bnk_union to Dsid_acc_bank_ws
%^		    Move Base_currency_id of Dsid_bnk_union to 
%^			Dsid_bank_curr_ws
%^		    Move Dsid_bank_curr_ws to Dsid_acc_curr_ws
%^		END-IF
%^	    END-IF
%^	END-IF.
%^	If (Disp_cur of Dsid_acctyp_seq NOT = SPACES )
%^	    Move Disp_cur of Dsid_acctyp_seq to Dsid_acc_curr_ws
%^	END-IF.	
%^        If (Dsid_acc_curr_ws = Dsid_currency_ws )
%^	    Set Success_is in Dsid_curr_okay to TRUE 
%^	ELSE
%^	    Set Failure_is in Dsid_curr_okay to TRUE 
%^	END-IF.
%^CHECK_TYP_CURRENCY_END.

%^   EXIT.
C520_CHECK_DUPE.
* Checks for a duplicate account.
	Move SPACES to Dsid_scr_adr_id.
	%Beg 
	Dsid_scr_adr_id = NULL ;
	NEXT: Dsid_acc_seq ;
	Dsid_scr_adr_id = Dsid_acc_seq.Rel_name_key;
	PREV: Dsid_acc_seq ;
	%End.
	If (Idtype of Dsid_scr_adr_id = Idtype of Dsid_know_acc_id )
	   AND (Idacc of Idkey of Dsid_scr_adr_id = Idkey of Dsid_know_acc_id )
	THEN
	    %Beg  Dsid_dupe_acc_ws = Dsid_know_acc_id ;  %End
	END-IF.

C520_CHECK_DUPE_END.



   EXIT.


C560_FIND_ACC_CUR.
%^ Scans through DBSA account sequence from present location (so we can use
%^  same paragraph to find ambiguities) looking for an account which is a
%^  currency match  (if the Account_type_ls was passed it it also checks
%^  for an account with that IDtype). Just checks current position and keeps 
%^  stepping until account currency matches message currency and the account type
%^  if specified matches the Idtype.
%^
	Set Failure_is in Dsid_curr_okay to TRUE.
	Set Failure_is in Dsid_type_okay to TRUE.
	PERFORM UNTIL (Success_is in Dsid_curr_okay AND Success_is in Dsid_type_okay) 
		      OR (Failure_is in Dsid_acc_seq_status   )
		      OR ( (Failure_is in Xbank_account_ok_ls )
                           AND ( Dbt_adr_bnk_id of Ent_debit_set NOT =
			      Idbank of Rel_name_key of Dsid_acc_seq ) )
	    PERFORM C480_CHECK_CURRENCY through C480_CHECK_CURRENCY_END
	    If (Dsid_idtype_ws = SPACES) or
	       (Idtype of Rel_name_key of Dsid_acc_seq = Dsid_idtype_ws) then
		Set Success_is in Dsid_type_okay to TRUE
	    End-if
	    If (Failure_is in Dsid_curr_okay or
		Failure_is in Dsid_type_okay)
		%^ reset BOTH to the flags
		Set Failure_is in Dsid_curr_okay to TRUE
		Set Failure_is in Dsid_type_okay to TRUE
                %Beg  NEXT: Dsid_acc_seq ;  %End
		Perform UNTIL ( (Failure_is in Dsid_acc_seq_status   )
			 	OR (Idtype of Rel_name_key 
					     of Dsid_acc_seq NOT = "P" ) )
		    %Beg  NEXT: Dsid_acc_seq;  %End
		END-PERFORM
	    END-IF
	END-PERFORM.


C560_FIND_ACC_CUR_END.

   EXIT.


C600_PUSH_DEBIT.

* Checks to make sure that there's an available debitside slot.
* Then calls PUSH_DEBIT_PARTY to push debit party down.

	If (obk_id of Obk of Ent_debit_set NOT = SPACES )
	    %^  No place to push into.
	    Set Failure_is in Dsid_ret_stat to true
	ELSE
	    CALL "PUSH_DEBIT_PARTY" USING
		By reference Bank of Loc_info of Ent_ftr_set
		By reference Dsid_pushed_debit
	      RETURNING Dsid_ret_stat
	END-IF.

	If Failure_is in Dsid_ret_stat   then
	    %^  No place to push into.
            %Beg
            Dsid_compose ^OUT(Dsid_err_memo)
                "No debitside slots for preferred corr ",
                    Dsid_preferred_corr, / ;
            %End
            Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
            Set Failure_is in Debit_look_account_status_ls to true
%^            Move 1 to Dsid_push_error
	    Set Success_is in Dsid_push_error To TRUE
            Set Failure_is in Nothing_suspicious_ls to true
            Set Success_is in Dsid_debit_acc_erred To TRUE
            %^ Reconnect the current Ent_d_adr_set if necessary
            If Dbt_rel_id of Ent_debit_set not = 0
                %Beg Dsid_conn_stat = Ent_d_adr_set State.conn ; %End
                If (Dsid_conn_stat = 0 )
                THEN
                    %Beg
                    Ent_debit_set.Dbt_adr_set_ptr CONN: Ent_d_adr_set(NOMOD) ;
		    Dsid_conn_stat = <1>;
                    %End
                END-IF
                %Beg Dsid_conn_ws = Relget_adr_set State.conn ; %End
                If (Dsid_conn_ws = 0 ) or
		   (Rel_id of Relget_adr_set not = Rel_id of Ent_d_adr_set)
                THEN
                    %Beg
        	    BREAK: Relget_adr_set;
        	    Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
		    Dsid_conn_ws = <1>;
                    %End
                END-IF
	      ELSE
		%Beg
		BREAK: Ent_d_adr_set;
		BREAK: Relget_adr_set;
		%End
		Move zeroes to Rel_id of Ent_d_adr_set
		Move zeroes to Rel_id of Relget_adr_set
		Move zeroes to Dsid_conn_stat
		Move zeroes to Dsid_conn_ws
	    END-IF

	END-IF.

C600_PUSH_DEBIT_END.



* Utility paragraphs.

   EXIT.
X900_ERROR_MEMO.
*  This paragraph writes an error memo using the text string in Dsid_err_memo.
	If Dsid_err_memo_length = 0 
	    GO TO X900_ERROR_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_err_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_err_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_err_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)
	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
	ADD 1 TO Error_memo_count_ls.
X900_ERROR_MEMO_END.


   EXIT.
X920_INFO_MEMO.
*  This paragraph writes an informational trace memo using the text string
*      in Dsid_info_memo.
	If Dsid_info_memo_length = 0 
	    GO TO X920_INFO_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_info_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_info_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_info_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)

	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
	ADD 1 TO Dsid_acc_info_count.
X920_INFO_MEMO_END.
	EXIT.

X930_CHANGE_DEBIT.
	%^
	%ACE_IS Relget_adr_set connected giving Dsid_conn_stat;
	%^
	%Beg
	BREAK: Prulepty_rule_seq(NOMOD);
	BREAK: Prulepty_party_adr_set(NOMOD);
	%End
	If Success_is in Dsid_conn_stat Then
		%beg Prulepty_source rel_is;
		     Prulepty_party_adr_ok Success_is;
	     	     Relget_adr_set.adr_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Relget_adr_set EQUATE: Prulepty_party_adr_set(NOMOD);
		%end
		If (Rel_id of Relget_adr_set = 0 )
		    %Beg  Prulepty_source aux_is;  %End
		end-if
	Else
		%beg Prulepty_source host_is;
		     Ent_Debit_set.Dbt_proc_rule CONN:
						       Prulepty_rule_seq(NOMOD);
		     BREAK: tmp_party_seq;
		     Prulepty_party_adr_ok Failure_is;
		%end
		Initialize Prulepty_party_adr_set
	end-if.

	%beg
	    Dsid_id_bank_ws = Ent_debit_set.dbt_adr_bnk_id;
	    Prulepty_bank_prof_rec = NULL;
	%end
	If (Dsid_id_bank_ws = SPACES)
	    %Beg  Dsid_id_bank_ws = Ent_ftr_set.Loc_info.Bank;  %End
	end-if

	If (Dsid_id_bank_ws = Bnk_id of Menu_bnk_union)
		%beg
		     Prulepty_bank_prof_rec =
					     Menu_bnk_union.Bnk_profile_id_rec;
		%end
	else
		%Beg
		   BREAK: Dsid_bnk_union ;
		   SEARCH: Bnk_index 
			   (Key = Dsid_id_bank_ws );
		%end
 		If (Success_is in Bnk_index_status)
			%Beg Bnk_index CONN: Dsid_bnk_union(NOMOD); %end
		end-if
		%beg
  		   Prulepty_bank_prof_rec = dsid_bnk_union.Bnk_profile_id_rec;
		%End
	end-if.

	Call "PRULE_CHANGE_DEBIT" returning Dsid_ret_stat.

X930_CHANGE_DEBIT_END.
	EXIT.

%^******************************************************************************
%^
%^      DEBIT_SCREEN_ACCOUNT
%^
%^******************************************************************************
%^
%^ Desired new features:
%^
%^	TODO - 
%^
%^
%^******************************************************************************
%^
%^ DEBIT_SCREEN_ACCOUNT routine.
%^
%^   Calling format:
%^	Call "Debit_screen_account" Using
%^              By Reference Init_debit_currency
%^		By Reference Debit_currency
%^		By Reference Message_amount
%^              By Reference Message_currency
%^		By Reference Debit_account
%^		By Reference Is_payment
%^		By Reference Xbank_account_ok
%^		By Reference Account_type
%^		By Reference Is_repetitive_lookup
%^		By Reference Nochange_bank
%^		By Reference Lock_dbt_party
%^		By Reference Special_fee_key
%^		By Reference Account_changed
%^		By Reference Debit_changed
%^		By Reference Debit_internal_state
%^		By Reference Currency_found
%^		By Reference Account_okay
%^		By Reference Nothing_suspicious
%^		By Reference Msg_bank_changed
%^		By Reference Error_Memo_count
%^	    Giving Return_status_ws.
%^	Possible return status values are:
%^		SUCCESS
%^		FAILURE
%^
%^
%^      Called from DEBITSIDE_SCREEN and from CREDITSIDE_SCREEN to process the
%^   debit party AIN SIs, set up the final debit party information, set up the 
%^   debit account, and set up the debit account information.
%^	Since the screen routines are interactive, we only want to process
%^   debit party SI's if the debit party has changed.  The Debit_changed
%^   argument indicates this.
%^	If the Debit_changed argument is non-0, we assume that the 
%^   DEBITSIDE_SCREEN routine has already tried to do a lookup on the debit
%^   party entered by the operator.  Ent_d_adr_set is assumed to be connected
%^   if this initial debit party  is on file.  We next check the Debit_locked
%^   argument; if it is zero we process AIN SI's pushing down or overwriting 
%^   the debit party if appropriate.
%^      Once we have found the ultimate debit party, the utility 
%^   SET_DEBIT_ADDRESS routine in SIDE_SUBS.COB is called to set debit party 
%^   elements from the address set.                   
%^      If either the Debit_changed argument or the Account_changed argument
%^   is non-0, we need to set the debit account.  This is done from the
%^   Debit_account argument if it is not SPACES; otherwise it is done from
%^   the debit address (which must be on-file) controlled by the Account_type
%^   argument (which dictates the type of account desired -- D, G, V, F, or P)
%^   and the Xbank_ok argument (if 0, the account MUST be in the same bank
%^   as the address).  The REL_ACC_FROM_ADR routine is used to get the 
%^   debit account and have the operator resolve any ambiguities in this
%^   process.
%^      If we are able to determine a debit account, we call the
%^   SET_DEBIT_ADDRESS routine in SIDE_SUBS.COB to set debit set elements from
%^   the account set.                                                         
%^      We will then execute any SI's which have been deferred to this point.
%^	If any significant database anomalies were found in processing the
%^    debit party, the status is returned as FAILURE.  Otherwise it will be
%^    returned as SUCCESS.
%^      If the initial debit party was on file and the account and address
%^    information were found without incident, the Nothing_suspicious argument
%^    will be returned non-0 to indicate that this message may continue
%^    through automated payment processing.
%^
%^ INPUT ARGUMENTS:
%^ explicit:
%^  Init_debit_currency       STR(3)  contains the initial debit currency.
%^      This is a mandatory field.
%^  Debit_currency	      STR(3)  contains the debit currency.
%^	This is a mandatory field.
%^  Message_amount	      DEC(14.2)  contains the message amount.
%^	This is a mandatory field.
%^  Message_currency          STR(3)  contains the message currency.  This
%^      is the currency trigger for SI's.
%^  Debit_account	      ACC_ID_REC.DDF contains account ID if caller
%^  	has pre-determined it; else spaces.
%^  Is_payment		      Long   If NON-0, message is a payment and 
%^	both an unambiguous debit address and debit account are required.
%^  Xbank_account_ok	      Long   Non-0 if we are permitted to find a
%^	debit account in a different bank from the debit party.
%^  Account_type	      STR(1) Select an account of this type from
%^ 	the accounts associated with the debit party address.
%^  Is_repetitive_lookup      Long   Non-0 if this is a repetitive lookup,
%^	in which case we will not map special instructions nor copy the
%^	debit account's cnf_seq to the message.
%^  Nochange_bank	      Long   Non-0 if a debit party bank which 
%^	disagrees with the Menu_bnk_union bank should not cause a bank
%^	context switch.  (The Msg_bank_changed flag will still be hoisted
%^	when appropriate even though no context switch will be done.)
%^  Lock_dbt_party            Long   Non-0 if the debit party is locked
%^      (such as by instantiation of a locked repetitive) and should not be
%^ 	changed by AINs, INTRTL tables, or anything else.
%^  Special_fee_key	      Str(1) Passed to Set_debit_account.  
%^				     SPACE for nothing special.
%^				     For now, "W" if the fees are to be waived.
%^  Account_changed	      Long   Non-0 if caller has set or changed the
%^  	debit account override or debit account idtype since the last time 
%^ 	we were called.
%^  Debit_changed	      Long   Non-0 if operator has changed the debit
%^	party since the last time we were called.
%^  Debit_internal_state      Long   Internal indicator of what state of 
%^	completion the pre-creditside part of debitside-lookup achieved.
%^  
%^ implicit:
%^   Debit party:
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^   SBK:
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^   Menu_bnk_union.Id for current bank id.
%^
%^
%^ OUTPUT ARGUMENTS:
%^explicit:
%^  Currency_found            Str(3) is the debit currency found -- the
%^	explicit currency of the debit account, if any.  It will only be SPACES
%^	if no debit account was found.
%^  Account_okay 	      long   is returned 0 if any mapping errors
%^	were detected during debit account lookup/setup, otherwise 1.
%^	Relget_msgcode contains detailed VMSG error message code.
%^  Nothing_suspicious	      long   is returned 0 if any mapping errors
%^	were detected during debitside setup, otherwise 1.  For payments,
%^	mapping errors are caused by any ambiguous debitside parties, a
%^	NOF debit party, or a non-existent or ambiguous debit
%^	account.  For non-payments, mapping errors are caused by any
%^	ambiguous debitside party or a NOF (but not non-existent) debit
%^	party.  If this flag is returned 0, the message should be routed to
%^	repair instead of being passed through for automated payment
%^	processing.
%^  Msg_bank_changed	      long   is returned non-0 if the debit party was
%^      found in a different bank, requiring the message (context) bank to
%^      change.
%^  Error_Memo_count          Long   is the number of error or warning
%^	memos added to the message history during debitside lookup. 
%^	Informational memos indicating normal SI parsing and execution are
%^	NOT error or warning memos.
%^  Return_status_ws is SUCCESS if no obviously bad or inconsistent
%^  rel or aux file data was read during debitside setup, otherwise FAILURE.
%^
%^Implicit:
%^   Message history
%^   Debit party:
%^	Ent_debit_set.Dbt_adr_set_ptr
%^	Ent_debit_set.Dbt_rel_id
%^	Ent_debit_set.Dbt_adr_bnk_id
%^	Ent_debit_set.Dbt_typ
%^	Ent_debit_set.Dbt_account
%^	Ent_debit_set.Dbt_name1		
%^	Ent_debit_set.Dbt_name2		
%^	Ent_debit_set.Dbt_name3		
%^	Ent_debit_set.Dbt_name4		
%^	Ent_debit_set.Dbt_acc_class
%^	Ent_debit_set.Dbt_acc_parent_code
%^	Ent_debit_set.Dbt_acc_prod_codes
%^	Ent_debit_set.Dbt_adr_class
%^	Ent_debit_set.Dbt_adr_type
%^	Ent_debit_set.Dbt_concen_acc
%^	Ent_debit_set.Dbt_currency
%^	Ent_debit_set.Dbt_department
%^	Ent_debit_set.Dbt_flag   
%^	Ent_debit_set.Dbt_recon_ref
%^	Ent_debit_set.Dbt_spc_inst1
%^	Ent_debit_set.Dbt_spc_inst2
%^	Ent_debit_set.Dbt_spc_inst3
%^	Ent_debit_set.Dbt_sys_of_rec
%^	Ent_debit_set.flgs.dbt_hold_flg
%^	Ent_debit_set.flgs.dbt_lim_flg  
%^	Ent_debit_set.flgs.dbt_ps_elig_flg
%^   SBK:
%^	Ent_debit_set.Sbk_adr_set_ptr
%^	Ent_debit_set.Sbk_adr_bnk_id
%^	Ent_debit_set.Sbk_rel_id
%^	Ent_debit_set.Sbk
%^	Ent_debit_set.Sbk_name1
%^	Ent_debit_set.Sbk_name2
%^	Ent_debit_set.Sbk_name3
%^	Ent_debit_set.Sbk_name4
%^   OBK:
%^	Ent_debit_set.Obk_adr_set_ptr
%^	Ent_debit_set.Obk_adr_bnk_id
%^	Ent_debit_set.Obk_rel_id
%^	Ent_debit_set.Obk
%^	Ent_debit_set.Obk_name1
%^	Ent_debit_set.Obk_name2
%^	Ent_debit_set.Obk_name3
%^	Ent_debit_set.Obk_name4
%^   ORP:
%^	Ent_debit_set.Orp_adr_set_ptr
%^	Ent_debit_set.Orp_adr_bnk_id
%^	Ent_debit_set.Orp_rel_id
%^	Ent_debit_set.Orp
%^	Ent_debit_set.Orp_name1
%^	Ent_debit_set.Orp_name2
%^	Ent_debit_set.Orp_name3
%^	Ent_debit_set.Orp_name4
%^
%^
%^ Modification history:
%^
%^	Fred P. Isaacs	7-FEB-1996
%^		Initial version.
%^      Fred P. Isaacs    15-MAY-1996
%^		Make sure that we leave account blank and ? in ovr for account
%^		map failure.
%^	Fred P. Isaacs	22-MAY-1996
%^		Changed args for SI_FIRST_DEBIT.
%^		Changed DEBIT_SCREEN_ACCOUNT args to have separate amount and 
%^		   currency passed by caller.
%^	Fred P. Isaacs	28-MAY-1996
%^              Removed deletion of department etc when reconnect fails.
%^	Fred P. Isaacs	31-MAY-1996
%^		Make sure that Relget_msgcode is cleared before each lookup
%^		step so we don't confuse an AUX lookup hit with a complete miss
%^	Fred P. Isaacs	17-JUN-1996
%^		Now clears PREadvise SI search status when we change debit
%^		parties.
%^	Fred P. Isaacs	20-JUN-1996
%^              Correct trap in debitside pushdown.
%^	Fred P. Isaacs	02-AUG-1996
%^              Indicate parse failure for pushdown error.
%^              Now behaves better for AIN trying to insert NOF party.
%^              Now objects to NOF account for on-file debit party,
%^		but leaves edits on NOF debit party to FTRSCR_EDITS.
%^	John R. Phelan	30-AUG-1996
%^		Clear out Dbt_recon_ref.  #19552
%^      John R. Phelan  02-SEP-1996
%^              Set the NOF Dbt_account correctly.  #19175, #19199, #19313.
%^      John R. Phelan  11-OCT-1996
%^              Fix trap when debitside AIN Alt/Acct is ambig or NOF.  #20935
%^      John R. Phelan  21-OCT-1996
%^              Bypass Debitside AIN's for source codes CHP and FED.  If an
%^              AIN is bypassed for this reason, a memo is written to
%^              the message history stating "Debit AIN bypassed because
%^              source code is xxx".  #21012
%^      John R. Phelan  30-DEC-1996
%^              Clear out debit party name and address when the address
%^              changes from on-file to not-on-file.  #21817
%^      John R. Phelan    23-JAN-1997
%^              If there's no room to insert a preferred correspondent,
%^              don't overwrite the debit party.  Also, make sure 
%^		address sets are always connected NOMOD.  #22302
%^      John R. Phelan    27-JAN-1997
%^              Add call(s) to SET_NOF_DBT_BNK_ID to determine not-on-file
%^              Dbt_adr_bnk_id based on various criteria.  #24397
%^      John R. Phelan   20-DEC-1996
%^              Set Account_okay_ls to 1 when account is not required.  #23664
%^      John R. Phelan   15-MAR-1997
%^              Change the search currency when the debit party is changed by
%^              an AIN or is selected from a Relget or Rel_acc_from_adr
%^              screen.  #23745
%^      John R. Phelan   15-MAR-1997
%^              Clear out the Dbt_hold flag from the previous account whenever
%^              the Dbt_account field is cleared.  #24455
%^      John R. Phelan   20-JUN-1997   
%^              If the first SI search fails, AND the address has no accounts,
%^              AND the debit_amount_currency_ls is NOT spaces, then use the
%^              debit_amount_currency_ls as the search currency for the
%^              second SI search.  #30139, #30395
%^      John R. Phelan    4-AUG-1997
%^              Don't use the Relget Disp_id from the disambiguation screen to
%^              determine the new search currency.  #29028
%^
%^	Fred P. Isaacs  4-SEP-1997	SPR 28833
%^		Currency changes.
%^
%^      John R. Phelan  3-FEB-1998
%^      35648   Changed to call the new Validate_pid_adv_type subroutine to
%^              determine the valid "P" source code, since this can now vary
%^              depending on bank.  This change is necessary for the Amex
%^              German Clearing Interface.
%^
%^	Fred P. Isaacs  3-MAR-1998	40074
%^	Cleaned up logic to recognize DSTATE_BAD status and act appropriately.
%^
%^	Fred P. Isaacs  16-MAR-1998	40556
%^	Return "nothing suspicious" non-zero if we did nothing.
%^
%^	Fred P. Isaacs 19-MAR-1998	 40649
%^		Make sure that account information is cleared if we change
%^		to an ABA and don't need an account.
%^
%^      John R. Phelan  13-APR-1998     41754
%^      Both Debitside_lookup and Debit_look_account were using the same
%^      field, Dsid_info_count.  Since Debitside_lookup can now call
%^      Debit_look_account directly, this was sometimes causing the
%^      info memo count to be doubled.  Debitside_lookup has been changed
%^      to use a field Dsid_adr_info_count and Debit_look_account uses
%^      Dsid_acc_info_count so they don't interfere with each other.
%^      Dsid_dbt_party_error has also been split into two fields for the
%^      same reason.
%^
%^	Fred Isaacs	10-SEP-1998	#45875
%^ 	Now changes ID in message to match debit account.
%^
%^	Fred P. Isaacs  16-APR-1999	53312 (PORT)
%^	Passes correct id length to REL_GET for inserted debit party.
%^
%^      Jan Walsh       02-FEB-1999     #51094
%^      Src_code "SEC" treated the same as src_code "FED".
%^
%^	Fred P. Isaacs	14-Oct-1999	58119 for port
%^	PID suffix will be checked on suffixed PID against source;
%^	suffix for source will be appended onto unsuffixed PID.
%^
%^	Fred P. Isaacs	15-Oct-1999	58231 for port
%^	Cleaned up Dsid_acc_id_ws usage and pushdowns.
%^
%^	Fred P. Isaacs	1-Nov-1999	58281 for port
%^	Initialize Currency_found_ls argument..
%^
%^	Fred Isaacs	20-JAN-2000	60544
%^	Clean up base id argument to have a zero length when its blank.
%^	COMPOSE will turn a null string into a single trailing space even if
%^	you turn off trailing spaces.
%^
%^	B. Lanza	16-Feb-2001	71057
%^	Debit Concentration account was not being filled in debit set.
%^	Add REG: and BREAK: for Ent_d_acc_rel_reg whenever the
%^	ent_d_acc_set is conn: or break: 
%^
%^	Ken Bjelke	28-Apr-2002	83550
%^	In B200_get_newparty added check for AIN replacing of account with same.
%^	This was occurring when the original and the replacement account share the
%^	same address. Account was filled in correctly, but error status was returned
%^	to caller due to too loop count exceeded.
%^
%^ Fred P. Isaacs  11-NOV-2002  90950
%^		Changed Prule_party_fsect to make AUX prules work.
%^
%^
%^ Ken Bjelke 23-Dec-2002	Spr 92061
%^		Correct logic around b280_find_account, An entered account
%^		number was being overwritten by Default.
%^
%^ Ken Bjelke 18-Feb-2003	Spr 95710
%^	Change logic for finding Account when non-accounting id specified, and
%^	no specific IDtype requested. 1st try for D in currency, Then Default,
%^	Then any in currency.
%^
%^	B. Lanza	12-Mar-2003	96468, 91584
%^	Duplicate account error on debit party. Address for dbt party had 2 
%^	accounts (1 in USD and 1 in foreign currency). SWF input msg in foreign 
%^	currency that has ABA for cdt party was unable to determine which acct
%^	to choose. Change made in A100_DO_ACCOUNT, for baseline currency, added
%^	"A" to cdt_typ's that allow message currency to be used as debit
%^	currency, which then determines the account to use.
%^
%^ Ken Bjelke 	05-May-2003	99480
%^	Finish correcting logic for Account Order
%^
%^ Fred P. Isaacs  7-AUG-2003  103339
%^	Changed level passed back by PRULE_MSG_RULE_MATCH to Dsid_pr_msglevel
%^	from Dsid_pr_level because it's the Prule_msglevel_oneof level type.
%^
%^ Ken Bjelke 	14-Aug-2003	103339, 103528
%^		Correct calling of PRULE_CHANGE_DEBIT. AINs were not being found.
%^
%^ Ken Bjelke 	08-Sep-2003	100369
%^	When account idtype is in use, attept to find account using the 
%^	Message Currency.
%^
%^ Ken Bjelke 	30-Sep-2003	105417
%^	Correct REL_ACC_FROM_ADR parameters. Was not finding correct account.
%^
%^ Ken Bjelke	01-Dec-2003	107682
%^	Correct setting of REL_ACC_FROM_ADR mode. There was a path thru the code
%^	that was allowing a prior (incorrect) value to be used.
%^
%^ Ken Bjelke	07-Jan-2004	108776
%^	Only prefix DBTAIN SUB parties with bank IF the dbt_adr_bank is 
%^	Different that thebank of the new party.
%^
%^ Ken Bjelke 	22-Jan-2004	109226	
%^	Connect AUX addresses also prior ro GET_NEWPARTY. Required to 
%^	detect DBTAIN's
%^
%^ Ken Bjelke 	2-Mar-2004	109809,110210
%^	Leave currency blank when searching for DBT account, when
%^ 	message_currency is set and fn_ena is not. Was causing bogus
%^	account not found in this currency message.
%^
%^ Ken Bjelke 	24-Mar-2004	110580
%^	Add perform of prule_change_debit to loop checking for new
%^	parties. Was only detecting the 1st AIN.
%^
%^ Ken Bjelke 	31-Mar-2004	111895
%^	 Inhibit the choosing of "L" accounts, as is done with
%^	 P and Y.
%^
%^ Ken Bjelke 21-Apr-2004	112177
%^	Do not use channel account for FED DRW (drawdown) Tran Types.
%^
%^ Ken Bjelke 	12-May-2004	95185,97006
%^	Add call to "CUST_BILAT_ACCT" to populate Bilateral account fields
%^	when required.
%^
%^ Ken Bjelke 	28-Jul-2004	115418
%^	Allow for AIN's above the address Level to trigger when Not
%^	an accounting IDtype
%^
%^ Ken Bjelke 1-Dec-2004	118663
%^ 	Compare the entire proposed DBTAIN party with the entire current
%^	prior to deciding whether this is a circular reference. 118049 did
%^	not cover all cases.
%^
%^
%^ Ken Bjelke 	02-Feb-2005  120320	
%^	Clear dbt party name and address prior to inserting after a DBTAIN.
%^	Info was left over if original was shorter than new.
%^
%^ Tom Carroll 	16-Nov-2005	125508, 127026, 127087 and 127127
%^	Synchronize the logic between DEBIT_SCREEN_ACCOUNT and DEBIT_LOOK_ACCOUNT
%^	when handling the mapping of the "Federal Reserve" account when the debit 
%^	party contains and ABA. 
%^
%^ Ken Bjelke 	23-Jan-2006	128297
%^	Skip killing in Dbt Account when the Source is FED and the tran_type is
%^	NON or RTN,  and msg_type is 1033.  This is a drawdown return. 
%^
%^ Ken Bjelke  2-Mar-2006	129168
%^	Add DRW to type that do not use the FED channel account. Clear
%^	Debit_account_ls to allow the dbt_account to populate the Dbt_id
%^	when appropriate.
%^
%^ Ken Bjelke 	17-Jul-2006	127532
%^	Generate a cross payment when FX_ENA and account is entered. Account should
%^	be sticky. (reverses some changes in 110210)
%^
%^ Migration:
%^ 	Ken Bjelke 	15-Mar-2006	129534
%^		Add call to GET_DBT_IDTYP_CHAN to populate the debit with a good IDtype
%^		based upon the source.
%^
%^ 	Ken Bjelke	10-Jan-2006	icr_001180
%^		reset the dbt_ovr in all DDR channel account cases. 
%^
%^	T. Welch	07-Mar-2007	icr_001555.. spr 135069
%^	When searching for an account in A120_DO_ACCOUNT paragraph and
%^	we have a message currency, no debit currency, no account identifier,
%^	and a cross-currency payment... there are two possible passes
%^	through the B280_FIND_ACCOUNT paragraph.  The first pass searches
%^	for an account under the message currency.  If none is found, a
%^	second pass searches for any account tied to the address with
%^	no currency specified.  An inappropriate 'Could not find account 
%^	in that currency' was being caught when the 2nd pass actually
%^	found an account.  Added logic to clear the error message upon
%^	a successful account search on the 2nd pass.
%^	  
%^
%^ R. Beer	12-Dec-2007	CR648, SPR 140521
%^	Unable to release transaction that was flagged for not-on-file debit
%^	party after new address and account records were added.
%^
%^
%^ Ken Bjelke 	14-May-2008	CR2725
%^	Allow FED account to populate the DBT_ACCOUNT for RTN's when the 
%^	Tran Type is NOT 1033.
%^
%^ Ken Bjelke	211-Mar-2009	CR7222
%^	allow entered account for FED source 1002/1007's. as return types.   
%^
%^ Ken BJelke 	11-MAr-2009	CR305
%^	Make entered account 'sticky'. Honor the entered Accounting type
%^	as typed, and edit agaisnty currency. Error if currency not matched. 
%^
%^ Ken Bjelke	20-May-2009	CR11239
%^	When entered type is 1002/1007 usr is trying a return, If the leave 
%^	A as the IDtype, popluate with the FED clearing. 
%^
%^ Ken Bjelke 	14-Dec-2009	tc14932   - Cr 141229
%^	Inhibit DBTAIN's from triggering on RTGS and Clearing_house items.
%^
%^ End revision history
%^******************************************************************************

%^******************************************************************************
%module DEBIT_SCREEN_ACCOUNT;

%^*****************************************************************************
%^                                                                            *
%^ Paragraphs in this procedure are labeled as follows:                       *
%^                                                                            *
%^	A100_MAIN	     Subroutine entry point, dispatch, and return
%^	B200_GET_NEWPARTY    Tries to get a pushdown/substitute party.
%^	B240_SET_ADDRESS     Sets up message debit party from address info.
%^	B280_FIND_ACCOUNT    Selects proper account from party address.
%^	B320_SET_ACCOUNT     Sets up message debit party from account info.
%^	C440_COPY_ADDRESS    Copies address info into debit party.
%^	C450_PUSH_DEBIT      Pushes current debit party down.
%^	X920_INFO_MEMO	     Writes an informational memo.
%^
*                                        				      *
%^*****************************************************************************
%^ Macro definitions.
%^ %MAR
%^ .default	displacement,long
%^ %end
%^*****************************************************************************
%linkage
01  Init_debit_currency_ls 		%Str(3);
01  Debit_currency_ls	  		%Str(3);
01  Message_amount_ls	  		%Amount;
01  Message_currency_ls   		%Str(3);
01  Debit_account_ls	  		%Str(34);
01  Is_payment_ls	  		%boolean;
01  Xbank_account_ok_ls	  		%boolean;
01  Account_type_ls  	  		%Str(1);
01  Is_rptv_lookup_ls	  		%boolean;
01  Nochange_bank_ls	  		%boolean;
01  Lock_dbt_party_ls	  		%boolean;
01  Special_fee_key_ls	  		%Str(1);
01  Account_changed_ls	  		%boolean;
01  Debit_changed_ls	  		%Long;
01  Internal_state_ls	  		%Long;
01  Currency_found_ls 	  		%Str(3);
01  Account_okay_ls	  		%boolean;
01  Nothing_suspicious_ls 		%boolean;
01  Msg_bank_changed_ls	  		%boolean;
01  Error_memo_count_ls	  		%Long;
01  Debit_screen_account_status_ls	%Boolean;

%Procedure using Init_debit_currency_ls, Debit_currency_ls, Message_amount_ls,
		Message_currency_ls, Debit_account_ls, Is_payment_ls, 
		Xbank_account_ok_ls, Account_type_ls, Is_rptv_lookup_ls, 
		Nochange_bank_ls, Lock_dbt_party_ls, Special_fee_key_ls,
		Account_changed_ls, Debit_changed_ls, Internal_state_ls,
		Currency_found_ls, Account_okay_ls, Nothing_suspicious_ls,
		Msg_bank_changed_ls, Error_memo_count_ls
	RETURNING  Debit_screen_account_status_ls.
%^*****************************************************************************

%^*****************************************************************************
A100_MAIN.
*  If there's nothing more to do, do it quickly and leave.
	set Failure_is in Msg_bank_changed_ls to true.
	Move Internal_state_ls to Dsid_internal.
	Move Spaces to dsid_account_type_ws.

	If (DSTATE_BAD in Dsid_internal )
	    Set Failure_is in Debit_screen_account_status_ls to true
	    Set Failure_is in Nothing_suspicious_ls to true
	    Move SPACES to Currency_found_ls
	    %EXIT PROGRAM
	END-IF.

	If (DSTATE_DONE in Dsid_internal )
%^ No further changes.  Just set up returns.
	    %Beg  Dsid_conn_ws = Ent_d_acc_set state.conn ;  %End
	    If (Dsid_conn_ws NOT = 0 )
		Move Currency_Id of Ent_d_acc_set to Currency_found_ls
	        If (Currency_found_ls = SPACES)
	        THEN
		    %Beg
		    BREAK: Dsid_bnk_union ;
		    SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
		    %End
		    If Success_is in Bnk_index_status  
		        %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		        Move Base_currency_id of Dsid_bnk_union to 
							      Currency_found_ls
		    END-IF
		END-IF
	    END-IF
	    Set Success_is in Debit_screen_account_status_ls to true
	    Set Success_is in Nothing_suspicious_ls to true
	    %EXIT PROGRAM
	END-IF.

*  Initialize returned variables.
	Set Success_is in Debit_screen_account_status_ls to true.
	Set Failure_is in Nothing_suspicious_ls to true.
	Move ZERO to Error_memo_count_ls.
	Set Success_is in Dsid_musthave_acct to true.
	Move SPACE to Dsid_ambig_ws 
	Move SPACE to Dsid_multibank_ws 
	Set Failure_is in Dsid_set_address to TRUE
	Set Failure_is in Dsid_set_account to TRUE
	Move SPACES to Dsid_preferred_corr
	%Beg  Dsid_preferred_corr = NULL ;  %End
	Set Failure_is in Dsid_debit_pend_del to TRUE.
%^	Move ZERO to Dsid_push_error.
	Set Failure_is in Dsid_push_error to TRUE.
	Move ZERO to Dsid_si_suspended.
	%Beg  Dsid_know_acc_id = NULL;  %End.
	Move SPACES to Currency_found_ls.
	If (Debit_account_ls NOT = SPACES )
            Move Debit_account_ls to Dsid_instring
	    %Beg
	    Dsid_clip_compose ^OUT(Dsid_temp1_vstr)
		Dsid_instring, / ;
	    Dsid_parse ^IN(Dsid_temp1_vstr)
		^OPTION( Dsid_know_acc_id.idbank(^STR<3>)),
		Dsid_intype_oneof( ^ONEOF(" ",
					"D",
					"G",
					"V",
					"F",
					"P" )),
		Dsid_know_acc_id.idkey(^STR<-1>), / ;
	   Dsid_compose ^OUT(Dsid_know_acc_id.idtype)
		Dsid_intype_oneof(^ONEOF(" ",
					"D",
					"G",
					"V",
					"F",
					"P" )), / ;
	   %End
	END-IF.
        Move Debit_currency_ls to Dsid_currency_ws.
	If DSTATE_INIT in Dsid_internal
	    Move 1 to Dsid_defer_adv_ws
	ELSE
	    Move ZERO to Dsid_defer_adv_ws
	END-IF

%^ Get the channel, to see if it is RTGS to allow us to bypass DBTAIN inserts. 
%^141229 - always re-read
%^

	Set ID_IS in dsid_prchan_mode to True
	If Dbt_adr_bnk_id of Ent_debit_set not = spaces then
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_debit_set.dbt_adr_bnk_id;
		%End
	Else
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_ftr_set.Loc_info.Bank;
		%End
	End-if

	%Beg dsid_chan_ident_ws.idkey = Ent_ftr_set.src_code; %End

	Call "GET_CHANNEL" using
			By Reference 	dsid_prchan_mode
			By Reference	dsid_chan_ident_ws
			By Reference 	dsid_chan_ident_ws_lengths
	Returning dsid_got_channel_ws.



	Move ZERO to Relget_msgcode.
	Move ZERO to Dsid_conn_stat.
	%Beg  BREAK: Dsid_acc_seq ;  %End.
 	If( (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE) 
  	    AND (Dbt_rel_id of Ent_debit_set NOT = 0 )) Or
  	    (dbt_adr_ptr_ok of flgs3 of ent_debit_set = "T")	
	then
*  Hook up address set - we will need it.
	    %Beg   Dsid_conn_stat = Ent_d_adr_set state.conn ;  %End
	    If Dsid_conn_stat = 0 Then
		If (Dbt_rel_id of Ent_debit_set NOT = 0 )
		    %ace_conn_root_q Rel_index ;
                    %Beg
		    BREAK: Relget_adr_set ;
		    Rel_index ^SEARCH (Key = Ent_debit_set.dbt_rel_id);
		    %end
		    If (Success_is in Rel_index_status   )
		       AND (OBJECT_IS in Rel_index_cursor )
		       AND (ADDRESS_IS in Rel_type of Rel_index)
                    THEN
		        %Beg  
			Rel_index CONN: Ent_d_adr_set(NOMOD) ;  
			Dsid_conn_stat = Ent_d_adr_set state.conn ;
		        %End
                    END-IF
		Else
			%^ AUX record, connect via ptr
			%beg
				Break: relget_adr_set;
				Ent_debit_set.dbt_adr_set_ptr CONN: ent_d_adr_set(nomod);
				dsid_conn_stat = ent_d_adr_set state.conn;
			%end
		END-IF
	    END-IF
	END-IF.
	If (Dsid_conn_stat = 0 )
* We really DON'T have a debit party.
            If (Dbt_rel_id of Ent_debit_set NOT = 0 )
                %Beg
                Ent_debit_set( .Dbt_name1          = NULL ,
                               .Dbt_name2          = NULL ,
                               .Dbt_name3          = NULL ,
                               .Dbt_name4          = NULL ,
			       .Dbt_res_country    = NULL) ;
                %End
                Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
                MOVE "N" to Dbt_comm_charge_ws
                MOVE "N" to Dbt_cbl_charge_ws
            END-IF
	    %Beg  
	    Ent_debit_set.dbt_typ.Dbt_ovr      = "*" ;		
	    Ent_debit_set( .Dbt_rel_id 	       = <0> ,
			   .Dbt_adr_set_ptr DELETE,
			   .flgs3.dbt_adr_ptr_ok = Null) ;
	    BREAK: Ent_d_adr_set ;
            %End
            Call "SET_NOF_DBT_BNK_ID" using
		by reference Nochange_bank_ls
                by reference Dsid_loc_bank_change
              RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		set success_is in Msg_bank_changed_ls to true
	    END-IF
            If Success_is in Msg_bank_changed_ls and Success_is in Nochange_bank_ls
                %^ Write an error memo indicating the discrepancy.
                %Beg
%^                Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOXBNKDBTPRIV";
		        field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
                %End
	        Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                Add 1 to Error_memo_count_ls
                Set Failure_is in Debit_screen_account_status_ls to true
            END-IF
            If Failure_is in Dsid_ret_stat   then
                %^ Write an error memo indicating the problem
  %^              %Beg  Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_INVBANKSTR";%End
		    %beg
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_INVBANKSTR" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                Add 1 to Error_memo_count_ls
                Set Failure_is in Debit_screen_account_status_ls to true
            END-IF

	    Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    GO TO A100_MAIN_EXIT

	ELSE
	    %Beg
	    Ent_d_adr_set.account_seq CONN: Dsid_acc_seq (NOMOD) ;
	    BREAK: Relget_adr_set ;
	    Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
	    %End
	    Set Failure_is in Dsid_didansi_ws to TRUE
	    IF (Success_is in Lock_dbt_party_ls)
		OR (Debit_changed_ls = 0 )
	    THEN
* Do not change the debit party
	    	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    	Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    	GO TO A100_MAIN_EXIT

	    END-IF
	    Perform B200_GET_NEWPARTY through B200_GET_NEWPARTY_END
	    If (Failure_is in Debit_screen_account_status_ls   )
		%^ No pref corr
		Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    	Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    	GO TO A100_MAIN_EXIT

	    END-IF
	END-IF.
	
	
%^A100_PUSH.
	Move ZERO to Relget_msgcode.
	Move 4 to Dsid_push_count.
	Perform UNTIL (Idtype of Dsid_preferred_corr = SPACE )
		       OR (Success_is in Dsid_push_error )
	    If (Dsid_push_count > 0 )
	        Subtract 1 from Dsid_push_count
	    ELSE
	    	%Beg
		Dsid_compose ^OUT(Dsid_err_memo)
		       "Preferred correspondent loop?? for "
			Dsid_preferred_corr, / ;
	 	%End
		Perform X920_INFO_MEMO through X920_INFO_MEMO_END
		Add 1 to Error_memo_count_ls
		Set Failure_is in Debit_screen_account_status_ls to true
		Set Failure_is in Nothing_suspicious_ls to true
		Set Success_is in Dsid_push_error to TRUE
	    	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    	Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    	GO TO A100_MAIN_EXIT
	    END-IF
	    Move SPACES to Dsid_acc_id_ws
	    %Beg  
	    Dsid_conn_stat = Ent_d_adr_set state.conn ;
            Dsid_acc_id_ws = Dsid_preferred_corr.idkey ;
	    %End
%^
%^ May need a different check, probably Channel info
%^
%^ We think it came from a clearinghouse.  Check this assumption.
%^ OBSELETE            Call "GET_PID_ADV_DATA" using
%^
	    If (Success_is in Dsid_itsa_clearhouse)
%^ For now, at least, this looks like an error.
	    	%Beg
		Dsid_compose ^OUT(Dsid_err_memo)
		       "Cannot execute Debit party change for clearinghouse "
			Ent_ftr_set.src_code, " message.", / ;
	 	%End
		Perform X900_ERROR_MEMO through X900_ERROR_MEMO_END
		Set Failure_is in Debit_screen_account_status_ls to true
                Set Success_is in Dsid_debit_acc_erred to TRUE
		Set Success_is in Dsid_push_error to TRUE
	    	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    	Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    	GO TO A100_MAIN_EXIT
	    END-IF

    	    %Beg  Dsid_next_function = Menu_next_function;  %End
	    %Beg  Menu_arg_cmd_vstr = "";  %End
	    Set DBT in Relget_title_flag to TRUE
	    Move "F" to Dsid_lkup_pend_del
	    Call "REL_GET" USING 
		By reference Idtype of Dsid_preferred_corr
		By reference Dsid_acc_id_ws
	        By reference Dsid_acc_id_ws_length
	        by reference Dsid_lkup_pend_del
	      RETURNING Dsid_ret_stat

%^	    If Success_is in Relget_ambiguous_wf 
%^	    Then
%^		Set Success_is in Resolve_ambiguous to True
%^		Set Dbt_lk_acc_Index 	in Dbt_scr_acc_state  to True
%^		Go to B260_DBT_LOOKUP2_END
%^	    End-if

	    If Relget_reject_flag = "M"
	    	Go to a100_main_end
	    End-if
	    If dsid_next_function not = Menu_next_function
		go to a100_main_end
	    end-if
	    If Dsid_next_function = Menu_next_function and
		    Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    	go to a100_main_end
	    End-if

	    If Dsid_lkup_pend_del = "T"
                %^	%Beg	Ftrscr.Debit.dbt_typ.dbt_id.Msg = "VMSG$_ADR_PEND_DEL"; %End
		    %beg
			field_ws = "DEBIT.DBT_TYP.DBT_ID";
			Mnemonic_ws = "VMSG$_ADR_PEND_DEL" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
                Add 1 to Error_memo_count_ls
	        Set Success_is in Dsid_debit_pend_del to TRUE
	        Set Failure_is in Debit_screen_account_status_ls to TRUE
            End-if
            If (PD_enqueue_to_server of Menu_cfg = "T")
	       AND (Relget_msgcode = Vmsg_lookup_fail_wc)
	       AND (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = "*")
	       AND (Failure_is in Dsid_debit_pend_del)
	    THEN
%^ We have an ambiguous address - write memo and let calling routine
%^  route message.
                %Beg
                Dsid_compose ^OUT(Dsid_info_memo)
                    "Ambiguous Address for Account: ",		     Ent_Debit_Set.Dbt_Typ.Dbt_Idtype, "/",
                     Ent_Debit_Set.Dbt_Typ.Dbt_Id, / ;
                %End
                Add 1 to Dsid_bogus_parties
                Perform X920_INFO_MEMO through X920_INFO_MEMO_END
            End-if
            If (Success_is in Dsid_ret_stat   )
	        AND (Dbt_ovr of Dbt_typ of Ent_debit_set = SPACE )
	        AND (Rel_id of Relget_adr_set = Rel_id of Ent_d_adr_set )
		AND (Idtype of Dsid_preferred_corr NOT = 
				"D" AND "G" AND "V" AND "F" AND "P" AND "A" )
		AND (Dsid_lkup_pend_del NOT = "T" )
	    THEN
		%Beg
	        Dsid_compose ^OUT(Dsid_err_memo)
		    		"Debit party is its own preferred corr ",
				Dsid_preferred_corr, / ; 
	        %End
		Perform X920_INFO_MEMO through X920_INFO_MEMO_END
		Add 1 to Error_memo_count_ls
		Set Failure_is in Debit_screen_account_status_ls to true
		Set Failure_is in Nothing_suspicious_ls to true
		Set Success_is in Dsid_push_error to TRUE
            ELSE
		If (Failure_is in Dsid_push_error )
	           AND (PUSHDOWN in Dsid_pushdown  )
	        THEN
                    %Beg
                    Dsid_conn_stat = Ent_d_adr_set state.conn ;
                    Dsid_conn_ws = Relget_adr_set state.conn ;
                    %End
		    If (Dsid_conn_ws NOT = 0 )
			%Beg
			BREAK: Dsid_adr_set ;
        		Relget_adr_set EQUATE: Dsid_adr_set(NOMOD) ;
			BREAK: Relget_adr_set ;
			%End
	            END-IF
		    If Dsid_conn_stat NOT = 0
			%Beg
			Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
			BREAK: Ent_d_adr_set;
	                %End
		      ELSE
			Move 0 to Rel_id of Relget_adr_set
		    END-IF
	            Perform C450_PUSH_DEBIT through C450_PUSH_DEBIT_END
		    If Success_is in Dsid_push_error then
	    		Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END
	    		Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END
	    		GO TO A100_MAIN_EXIT
		    END-IF
		    %Beg  BREAK: Relget_adr_set ;  %End
                    If (Dsid_conn_ws NOT = 0 )
			%Beg
			Dsid_adr_set EQUATE: Relget_adr_set(NOMOD) ;
			BREAK: Dsid_adr_set ;
			%End
		    ELSE
%^ We must repeat the lookup.
		        Move "F" to Dsid_lkup_pend_del
	     	        %Beg  Dsid_next_function = Menu_next_function;  %End
			%Beg  Menu_arg_cmd_vstr = "";  %End
	    		Set DBT in Relget_title_flag to TRUE
	    		Call "REL_GET" USING 
	  		    By reference Idtype of Dsid_preferred_corr
			    By reference Dsid_acc_id_ws
			    By reference Dsid_acc_id_ws_length
	        	    by reference Dsid_lkup_pend_del
			  RETURNING Dsid_ret_stat
		    END-IF
		END-IF
		If Relget_reject_flag = "M"
	    	    Go to A100_main_end
		End-if
		If Dsid_next_function not = Menu_next_function Then
		        Go to A100_main_end
		End-if
		If Dsid_next_function = Menu_next_function and
			Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    	    Go to A100_main_end
		End-if
	        If (Success_is in Dsid_ret_stat   )
	           OR (Relget_msgcode = Vmsg_dat_notonfile_wc ) 
	        THEN
* We actually did get a REL or AUX db hit, so let's copy the address.
	            Perform C440_COPY_ADDRESS through C440_COPY_ADDRESS_END
	        END-IF
	        If (Relget_return_key NOT = SPACES )
		    %Beg
		    Dsid_parse ^IN(Relget_return_key), Dsid_return_key, 
			^SPACE, / ;
		    %End
	        ELSE
		    %Beg  Dsid_return_key = NULL ;  %End
	        END-IF	
	        If (Failure_is in Dsid_ret_stat   )
	            If (Success_is in Dsid_debit_pend_del)
		        %Beg
			Ent_debit_set.dbt_typ.dbt_ovr = "?";

			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_ADR_PEND_DEL" ;
	                Dsid_compose ^OUT(Dsid_info_memo)
		    		"ERROR: Preferred corr ",
				Dsid_preferred_corr, " for ",
				Ent_debit_set.dbt_typ, " is pend del", / ; 
			%End
		    	call "FTRSCR_WRITE_ERROR" using
				by reference field_ws
				by reference Mnemonic_ws
	            ELSE
		    %Beg
			Ent_debit_set.dbt_typ.dbt_ovr = "*" ;
	                Dsid_compose ^OUT(Dsid_info_memo)
		    		"ERROR: NOF preferred corr ",
				Dsid_preferred_corr, " in SI for ",
				Ent_debit_set.dbt_typ, / ; 
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_MR_CORRNOTONFILE" ;
                    %End
	      	    call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
	            END-IF
	            Move SPACES to Dbt_country_code_ws
	            Perform X920_INFO_MEMO through X920_INFO_MEMO_END
	            Set Failure_is in Debit_screen_account_status_ls to true
		    Set Success_is in Dsid_push_error to TRUE
		    Add 1 to Error_memo_count_ls
	            %Beg
	            Ent_debit_set(.Dbt_typ 
				   (.Dbt_ovr    = "*",
				    .Dbt_idtype = Dsid_preferred_corr.Idtype,
				    .Dbt_id     = Dsid_preferred_corr.Idkey ),
				  .Dbt_name1 = NULL,
				  .Dbt_name2 = NULL,
				  .Dbt_name3 = NULL,
				  .Dbt_name4 = NULL,
				  .Dbt_res_country = NULL);
	            %End
                    Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
                    MOVE "N" to Dbt_comm_charge_ws
                    MOVE "N" to Dbt_cbl_charge_ws
            	    Call "SET_NOF_DBT_BNK_ID" using 
			by reference Nochange_bank_ls
	                by reference Dsid_loc_bank_change
	              RETURNING Dsid_ret_stat
		    If (Dsid_loc_bank_change NOT = 0 )
			set success_is in Msg_bank_changed_ls to true
		    END-IF
        	    If (success_is in Msg_bank_changed_ls)
		       AND (Success_is in Nochange_bank_ls)
		    THEN
                	%^ Write an error memo indicating the discrepancy.
%^                Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOXBNKDBTPRIV";
	                %Beg
			        field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
				Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
	                %End
	       	 	Call "FTRSCR_WRITE_ERROR" using
				by reference field_ws
				by reference Mnemonic_ws
	                Add 1 to Error_memo_count_ls
        	        Set Failure_is in Debit_screen_account_status_ls to true
	            END-IF
        	    If Failure_is in Dsid_ret_stat   then
%^                Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_INVBANKSTR";
 	               %Beg
				field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
				Mnemonic_ws = "VMSG$_INVBANKSTR";
	  	        %End
		        Call "FTRSCR_WRITE_ERROR" using
				by reference field_ws
				by reference Mnemonic_ws
        	        Add 1 to Error_memo_count_ls
                	Set Failure_is in Debit_screen_account_status_ls to true
	            END-IF
 	        ELSE
		    If (Success_is in Dsid_didansi_ws)
 		        %Beg
		        Dsid_compose ^OUT(Dsid_info_memo)
				"Debit party ", Ent_debit_set.dbt_typ,
				Dsid_pushdown (^oneof(" Rule substituted ", 
						      " Rule inserted ")),
				Dsid_preferred_corr, / ;
		        %End
		        Perform X920_INFO_MEMO through X920_INFO_MEMO_END
                    END-IF
 		    If (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = SPACE )
	                %Beg  Ent_debit_set.Dbt_typ.Dbt_ovr = " " ;  %End
		    END-IF
	            %Beg
	            Ent_debit_set.Dbt_typ.Dbt_idtype = Relget_return_idtype ;
		    %End
		    If (Dsid_id_bank_ws_length NOT = 0 )And
		       (Dsid_id_bank_ws NOT = Dbt_adr_bnk_id of Ent_debit_set) %^ 108766
		    Then
		        %Beg
		        Dsid_compose ^OUT(Ent_debit_set.dbt_typ.dbt_id )
			        Dsid_id_bank_ws, ":", Dsid_return_key, / ;
		        %End
		    ELSE 
	                %Beg
	                Ent_debit_set.Dbt_typ.Dbt_id = Dsid_return_key ;
	                %End
		   END-IF
		   %Beg
		   BREAK: Ent_d_adr_set;
                   Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;
		   Dsid_conn_stat = <1>;
                   Ent_debit_set(  .Dbt_rel_id = Ent_d_adr_set.Rel_id,
                                   .Dbt_adr_set_ptr POINT: Ent_d_adr_set,
			   	   .flgs3.dbt_adr_ptr_ok = "T" );
		   %End
		   Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END
	           Perform B200_GET_NEWPARTY through B200_GET_NEWPARTY_END
	        END-IF		    
	    END-IF
	END-PERFORM.

* We have either pushed down through all preferred correspondents or had
* an error doing so.


	Perform A110_GOT_DEBIT through A110_GOT_DEBIT_END.

	Perform A120_DO_ACCOUNT through A120_DO_ACCOUNT_END.


A100_MAIN_EXIT.
%^
%^ At this point, we have a Channel and a good Account, check for the Bilat
%^
	Call "CUST_BILAT_ACCT".


	If (Failure_is in Is_payment_ls)
* For non-payments, we don't require a debit account
	    If (Dsid_bogus_parties = 0 )
		AND (Failure_is in Dsid_push_error )
	    THEN
* Debit party was completely processed.
	        Set Success_is in Nothing_suspicious_ls to true
	    END-IF
	ELSE
* Payment, so we must look at debit account as well.
	    If (Debit_changed_ls NOT = 0 )
		If Failure_is in Debit_screen_account_status_ls  
	    	    Set DSTATE_BAD in Dsid_internal to TRUE
		ELSE
		    If (Dsid_si_suspended NOT = 0 )
			Set DSTATE_HOLD in Dsid_internal to TRUE
		    ELSE
			Set DSTATE_DONE in Dsid_internal to TRUE
		    END-IF
		END-IF
		%Beg  Dsid_intern_word = Dsid_internal ;  %End
		Move Dsid_intern_word to Internal_state_ls

		If ( (Success_is in Dsid_set_address)
		   AND (Failure_is in Dsid_set_account) )
		THEN
		    Set Failure_is in Nothing_suspicious_ls to true
		ELSE
		    Set Success_is in Nothing_suspicious_ls to true
		    If (Dsid_bogus_parties NOT = 0 )
		       OR (Success_is in Dsid_push_error)
		    THEN
* Debit party was completely processed.
	                Set Failure_is in Nothing_suspicious_ls to true
		    END-IF
	        END-IF
	    ELSE
		If (Success_is in Account_changed_ls)
		    If Failure_is in Debit_screen_account_status_ls  
	    	        Set DSTATE_BAD in Dsid_internal to TRUE
		    ELSE
		        If (Dsid_si_suspended NOT = 0 )
			    Set DSTATE_HOLD in Dsid_internal to TRUE
		        ELSE
			    Set DSTATE_DONE in Dsid_internal to TRUE
		        END-IF
		    END-IF
		    %Beg  Dsid_intern_word = Dsid_internal ;  %End
		    Move Dsid_intern_word to Internal_state_ls

		    If ( (Success_is in Dsid_set_account)
			  OR (Failure_is in Dsid_musthave_acct   ) )
			AND (Dsid_bogus_parties = 0 )
			AND (Failure_is in Dsid_push_error )
		    THEN
* Account was processed
	                Set Success_is in Nothing_suspicious_ls to true
		    END-IF
		ELSE
		    If (Dsid_bogus_parties = 0 )
			AND (Failure_is in Dsid_push_error )
		    THEN
* Debit party not processed; no others failed.
			Set Success_is in  Nothing_suspicious_ls to true
		    END-IF
		END-IF
	    END-IF
	    If (Success_is in Account_changed_ls)
		OR (Debit_changed_ls = 1 )
		AND (Failure_is in Dsid_set_account)
		AND (Success_is in Dsid_musthave_acct   )
	    THEN
		Move SPACES to Dbt_account of Ent_debit_set
		%Beg  
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
	        %End
		If ( (Success_is in Dsid_set_address)
		     OR (Dbt_rel_id of Ent_debit_set NOT = 0 ) )
		THEN
		    If (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = "?" )
		        %Beg  Ent_debit_set.Dbt_typ.dbt_ovr = "?" ;  %End
		    END-IF
		    Set Failure_is in Debit_screen_account_status_ls to true
		END-IF
	    END-IF
	END-IF.

	%Beg
	BREAK: Dsid_acc_seq ;
	%End.

        If Dbt_ovr of Ent_debit_set not = spaces then
                Call "SET_NOF_DBT_ACCOUNT"
        end-if.
	%^
	%^ Update Debit Party
	%^
	%beg dsid_upd_level dbt_party_is; %end
	Call "PRULE_UPDATE_PARTY" Using
		By Reference  dsid_upd_level
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.


A100_MAIN_END.
        %EXIT PROGRAM.


A110_GOT_DEBIT.

	If (Debit_changed_ls not = 1 )
            GO TO A110_GOT_DEBIT_END
	END-IF.
	Move SPACES to Currency_found_ls
	IF (Dbt_ovr of Dbt_typ of Ent_debit_set NOT = SPACE )
* NOF debit party.  Set debit party bank to something reasonable.
            If (Dbt_rel_id of Ent_debit_set NOT = 0 )
                %Beg
                Ent_debit_set( .Dbt_name1          = NULL ,
                               .Dbt_name2          = NULL ,
                               .Dbt_name3          = NULL ,
                               .Dbt_name4          = NULL ,
			       .Dbt_res_country    = NULL) ;
                %End
                Call "ACCTSUB_DBT_NOF"  %^ Hose out other debit fields.
                MOVE "N" to Dbt_comm_charge_ws
                MOVE "N" to Dbt_cbl_charge_ws
            END-IF
            Call "SET_NOF_DBT_BNK_ID" using 
		by reference Nochange_bank_ls
                by reference Dsid_loc_bank_change
              RETURNING Dsid_ret_stat
	    If (Dsid_loc_bank_change NOT = 0 )
		set success_is in Msg_bank_changed_ls to true
	    END-IF
      	    If Success_is in Msg_bank_changed_ls and Success_is in Nochange_bank_ls
                %^ Write an error memo indicating the discrepancy.
%^	        %Beg  	Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOXBNKDBTPRIV";%End

	        %Beg
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
	        %End
	       	Call "FTRSCR_WRITE_ERROR" using
				by reference field_ws
				by reference Mnemonic_ws
		Add 1 to Error_memo_count_ls
        	Set Failure_is in Debit_screen_account_status_ls to true
	    END-IF
       	    If Failure_is in Dsid_ret_stat   then
	        %^ Write an error memo indicating the problem
%^        	%Beg  Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_INVBANKSTR" %End
       	        %Beg
			field_ws = "DEBIT.DBT_TYP.DBT_ITYPE";
			Mnemonic_ws = "VMSG$_INVBANKSTR";
               	%End
	                Call "FTRSCR_WRITE_ERROR" using
				by reference field_ws
				by reference Mnemonic_ws
        	Add 1 to Error_memo_count_ls
                Set Failure_is in Debit_screen_account_status_ls to true
	    END-IF
	    If (Dbt_ovr of Dbt_typ of Ent_debit_set = "*")
		AND (Fx_ena of Menu_cfg = LOW-VALUES )
	    THEN
		%Beg
		BREAK: Dsid_bnk_union ;
		SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
		%End
		If Failure_is in Bnk_index_status  
		    Move SPACES to Currency_found_ls
		ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Base_currency_id of Dsid_bnk_union to 
			Currency_found_ls
		END-IF
	    END-IF
	    %Beg
	    Dsid_internal DSTATE_DONE ;
	    Dsid_intern_word = Dsid_internal ;
	    %End
	    Move Dsid_intern_word to Internal_state_ls
	ELSE
	    Perform B240_SET_ADDRESS through B240_SET_ADDRESS_END
	END-IF.

* Since we have a new debit party, reset preadvise SI status.
	%Beg
	Ent_ftr_set.Si_flgs.Pre_found = NULL ;
	Ent_debit_set.Flgs.Dbt_ps_elig_flg = NULL ;
	%End.


	Call "GET_DBT_IDTYP_CHAN" Using
		By Reference Dsid_temp_idtype,
		By Reference Dsid_lookup_temp,
		By Reference Dsid_lookup_temp_length
	Returning dsid_ret_stat.

	If Success_is in dsid_ret_stat Then
		%^ have have a good ID use it
		%beg ent_debit_set.dbt_typ.dbt_idtype = Dsid_temp_idtype;
  		     Dsid_compose ^OUT(Ent_Debit_set.dbt_typ.dbt_id)
				    Dsid_lookup_temp,/;
		%end
	end-if.

* Just in case we have a debit party bank change and context switches
*  are disabled.
	If ( Dbt_adr_bnk_id of Ent_debit_set NOT = SPACES )
	   AND (Dbt_adr_bnk_id of Ent_debit_set NOT = 
					      Bank of Loc_info of Ent_ftr_set )
	THEN
	    %Beg
	    Dsid_id_ws = Ent_debit_set.Dbt_typ.Dbt_id;
	    Dsid_Compose ^Out(Ent_debit_set.dbt_typ.dbt_id)
			Ent_debit_set.Dbt_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.



	Call "GET_DBT_IDTYP_CHAN" Using
		By Reference Dsid_temp_idtype,
		By Reference Dsid_lookup_temp,
		By Reference Dsid_lookup_temp_length
	Returning dsid_ret_stat.

	If Success_is in dsid_ret_stat Then
		%^ have have a good ID use it
		%beg ent_debit_set.dbt_typ.dbt_idtype = Dsid_temp_idtype;
  		     Dsid_compose ^OUT(Ent_Debit_set.dbt_typ.dbt_id)
				    Dsid_lookup_temp,/;
		%end
	end-if.

	Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END.

A110_GOT_DEBIT_END.
    EXIT.

A120_DO_ACCOUNT.
	If (Failure_is in Account_changed_ls)
	   AND (Debit_changed_ls = 0 )
	THEN
	    GO TO A120_DO_ACCOUNT_END
	END-IF.
	Perform B300_GET_ACC_FROM_CHAN thru
		B300_GET_ACC_FROM_CHAN_END

	If dsid_know_acc_id not = Spaces  AND
	  NOT(Src_code of Ent_ftr_set  = "FED" and
	       ((tran_type of ent_ftr_set = "NON" or "DRW") And %^ Non accounting FED, DRW get different acct 
		 tran_type of ent_ftr_set = "RTN" AND type_code of ent_ftr_set(3:2) = "33"
 	       ) OR		 %^ RTN of a Drawnrequest gets a different account also
	        (Src_code of Ent_ftr_set  = "RTS" and
	         incoming_msgtype of ent_ftr_set = "198")
	      )
	Then
		%Beg BREAK: Ent_d_acc_set; %End
		Perform B310_CONN_ACCOUNT through B310_CONN_ACCOUNT_END
		Perform X930_CHANGE_DEBIT thru X930_CHANGE_DEBIT_END
		Go to A120_DO_ACCOUNT_END
	End-if.

%^	If (Dbt_idtype of Dbt_typ of Ent_debit_set = "A" )
%^	   AND (Src_code of Ent_ftr_set = "FED" OR "SEC")
	If (Success_is in Dsid_itsa_clearhouse) And
	  Not(Src_code of Ent_ftr_set = "FED" and   %^ 112177
	     (( tran_type of Ent_ftr_set = "DRW") ) Or  %^ Still get account for Drawdowns
	      (type_code of Ent_ftr_set = "1002" or "1007")) %^ or manual returns.    
	THEN
%^ It's an incoming fed, so we don't need an account.
%^ We no longer need an account for any clearing house types
%^ Account has been set in the mapper already
%^
            Set Failure_is in Dsid_musthave_acct to true
            Set Success_is in Account_okay_ls to true
		%^    Move "USD" to Currency_found_ls
	    Set DSTATE_DONE in Dsid_internal to TRUE
	    %Beg  Dsid_intern_word = Dsid_internal ;  %End
	    Move Dsid_intern_word to Internal_state_ls
		%^	    If (Dbt_account of Ent_debit_set NOT = SPACES )
		%^               %Beg
		%^		Ent_debit_set (.Dbt_account        = NULL,
		%^                              .Dbt_acc_class      = NULL,
		%^                               .Dbt_acc_prod_codes = NULL,
		%^                               .Flgs.dbt_hold_flg  = NULL,
		%^                               .Dbt_recon_ref      = NULL ) ;
		%^		BREAK: Ent_d_acc_set ;
		%^		BREAK: Ent_d_acc_rel_reg;
		%^		%End
		%^	    END-IF
	    GO TO A120_DO_ACCOUNT_END
	END-IF.
	%Beg  BREAK: Ent_d_acc_set ;  %End
	Set Failure_is in Account_okay_ls to true
	If (Dbt_rel_id of Ent_debit_set = 0 )
	   AND (Dsid_know_acc_id = SPACES )
	THEN
* NOF debit account.
	    GO TO A120_DO_ACCOUNT_END
	END-IF.

	If (Debit_currency_ls NOT = SPACES )
%^ Search currency was set by caller
	    Perform B280_FIND_ACCOUNT through B280_FIND_ACCOUNT_END
	ELSE
	    If Message_currency_ls = SPACES 
		Perform B280_FIND_ACCOUNT through B280_FIND_ACCOUNT_END
	    ELSE
%^ Message currency is set but debit currency was not.
		If (Dbt_idtype of Dbt_typ of Ent_debit_set =
					      "D" OR "V" OR "G" OR "P" OR "F" )
		    %^  If NOT(FX_ENA of Menu_cfg = LOW-VALUES) Then  	%^ 110210
		    %^	Move Message_currency_ls to dsid_currency_ws	%^ 100369
		    %^  end-if
		    Perform B280_FIND_ACCOUNT through B280_FIND_ACCOUNT_END
		ELSE
%^ Not an account identifier
		    If (Fx_ena of Menu_cfg = LOW-VALUES )
%^ Baseline currency environment
			If (Cdt_idtype of Cdt_typ of Ent_credit_set =
						     "D" OR "V" OR "G" OR "P" OR
				 (Cdt_idtype of Cdt_typ of Ent_credit_set = "A" 
				   AND Base_currency_id of Menu_bnk_union = "USD") )
			    Move Message_currency_ls to Dsid_currency_ws
                        ELSE
			    If (Cdt_idtype of Cdt_typ of Ent_credit_set =  "F" )
				%Beg
				BREAK: Dsid_bnk_union ;
				SEARCH: Bnk_index 
					(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
				%End
				If Success_is in Bnk_index_status  
				    %Beg  
				    Bnk_index CONN: Dsid_bnk_union (NOMOD) ;
				    %End
		    		    Move Base_currency_id of Dsid_bnk_union
							 to Dsid_currency_ws
				END-IF
			    END-IF
			END-IF
		    	Perform B280_FIND_ACCOUNT through B280_FIND_ACCOUNT_END
                    ELSE
%^ Cross-currency environment.  Try the message currency.
			Move Message_currency_ls to Dsid_currency_ws
			Perform B280_FIND_ACCOUNT through B280_FIND_ACCOUNT_END
			If (Dsid_know_acc_id = SPACES )
%^ That didn't work.  Try no preference and defaulting.
			    Move SPACES to Dsid_currency_ws
                            Perform B280_FIND_ACCOUNT through 
							B280_FIND_ACCOUNT_END
				%^ If find account on 2nd pass,
				%^ clear previous error from 1st B280 pass.
			    If (Dsid_know_acc_id not = SPACES)
				%beg
				field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
				Mnemonic_ws = "";
        			%end
				call "FTRSCR_WRITE_ERROR" using
				    by reference field_ws
				    by reference Mnemonic_ws
			    End-if
			END-IF
		    END-IF
		END-IF			    	
	    END-IF
	END-IF.
	If (Idtype of Dbt_account of Ent_debit_set NOT = SPACE )
	    Perform B320_SET_ACCOUNT through B320_SET_ACCOUNT_END
	    Move Currency_id of Ent_d_acc_set to Currency_found_ls
	    If (Currency_found_ls = SPACES)
	    THEN
	        %Beg
	        BREAK: Dsid_bnk_union ;
	        SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
	        %End
	        If Failure_is in Bnk_index_status  
		    Move SPACES to Currency_found_ls
	        ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Base_currency_id of Dsid_bnk_union to Currency_found_ls
	        END-IF
	    END-IF
	ELSE
	    If (Fx_Ena of Menu_cfg = LOW-VALUES)
	    THEN
	        %Beg
	        BREAK: Dsid_bnk_union ;
	        SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
	        %End
	        If Failure_is in Bnk_index_status  
		    Move SPACES to Currency_found_ls
	        ELSE
		    %Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		    Move Base_currency_id of Dsid_bnk_union to Currency_found_ls
	        END-IF
	    ELSE
	        Move SPACES to Currency_found_ls
	    END-IF
	END-IF.

A120_DO_ACCOUNT_END.
    EXIT.

B200_GET_NEWPARTY.
* Checks SI chain to see if we have a new credit party.
	Set Failure_is in Dsid_didansi_ws to TRUE
        Move 1 to Dsid_number_SIs
	Move SPACES to Dsid_previce_vstr

	If (Success_is in Dsid_got_channel_ws) and
	   ((clrhouse_is in Endpoint_type of Rchan_channel_set) Or
	    (RTGS_is in Endpoint_type of Rchan_channel_set))
	Then
	   %^ NO DBTAINS for RTGS systems
	    Move SPACES to Dsid_preferred_corr
	    %Beg  Dsid_preferred_corr = NULL ;  %End
	    Go to B200_GET_NEWPARTY_END
	End-if.

	%Beg 
	Dsid_SI_types = "AIN" ;  
	Dsid_previce_vstr = NULL;
	Dsid_pr_ordinal_ws = <0>;
	%End.
	%beg dsid_compose ^out(dsid_pr_type_ws) "DBTAIN",/;%end
	Call "PRULE_MSG_RULE_MATCH" Using
		By Reference dsid_pr_type_ws
		by Reference dsid_pr_type_ws_length
		by Reference dsid_pr_ordinal_ws
		by Reference dsid_pr_msglevel
		By Reference dsid_pr_source
		by reference dsid_pr_subtype_ws
		by reference dsid_pr_subtype_ws_length
		By Reference dsid_pr_memo
		by reference dsid_pr_memo_length
	returning Dsid_ret_stat.

	If Success_is in Dsid_ret_stat And
 	  (dbt_idtype of dbt_typ of Ent_debit_set =
   	   "D" OR "G" OR "F" OR "V" ) 
	Then
		If ( NOT ( (Msg_ml in Dsid_pr_msglevel)
		        OR (Dbt_msg_ml in Dsid_pr_msglevel )
		        OR (Dbt_db_ml in Dsid_pr_msglevel) ) )
			%^ No AINS above address/message level
			Set Failure_is in dsid_ret_stat to True
		end-if
	end-if.

        If (Success_is in Dsid_ret_stat   )
%^ Found a matching AIN SI.
	    Call "IS_PID_ADV_TYPE" Using
		By reference Bank of Loc_info of Ent_ftr_set
		By Reference Src_code of Ent_ftr_set
	      Returning Dsid_ret2_stat
            If (Src_code of Ent_ftr_set = "FED" OR "SEC"
		or Success_is in Dsid_ret2_stat)
	    THEN
		%Beg
                Dsid_compose ^OUT(Dsid_info_memo)
                    "Debit AIN bypassed because source code is ",
                    Ent_ftr_set.src_code, /;
                %End
                Perform X920_INFO_MEMO through X920_INFO_MEMO_END
                Go to B200_GET_NEWPARTY_END
            END-IF

	    %^
	    %^	Expand the Returned AIN 
	    %^	
	    Perform GET_AIN_PARAMS thru
		    GET_AIN_PARAMS_END

	    %^ 83550 - was looping.
            If (idtype of dsid_preferred_corr =
		dbt_idtype of dbt_typ of ent_debit_set) Then
		    If  idtype of dsid_preferred_corr = "D" or "F" or "V" or "G"  Then
			%^ If our bank, strip for the comparision
			Move dbt_id_length of Ent_debit_set_lengths to dsid_tmp_fin_pos_ws
			If dbt_id of dbt_typ of ent_debit_set(dsid_tmp_fin_pos_ws:1) = "/"
			Then	%^ Discount the trailing / if present
					Subtract 1 from dbt_id_length of Ent_debit_set_lengths giving
					dsid_tmp_fin_pos_ws
			End-if 
			%^ First, if replacing with a larger key, don't bother checking
			%^ for dup
			%^ If our bank, strip for the comparision
				If idkey of dsid_preferred_corr(1:3) =  bnk_id of menu_bnk_union
			Then
				Move 5 to dsid_tmp_start_pos_ws
			Else
				Move 1 to dsid_tmp_start_pos_ws
			end-if
			Move idkey_length of dsid_preferred_corr_lengths to dsid_tmp_corr_len_ws
		        If (idkey of dsid_preferred_corr(dsid_tmp_start_pos_ws:dsid_tmp_corr_len_ws) =
			     dbt_id of dbt_typ of ent_debit_set(1:dsid_tmp_fin_pos_ws))
			Then
			    %^Check in length -1 due to addition of / at end of account number
			    %^ Bypass, we have a circular reference, replacing
			    %^ same with same.
			    Move SPACES to Dsid_preferred_corr
			    %Beg Dsid_preferred_corr = NULL ;  %End
			    Go to B200_GET_NEWPARTY_END
			end-if
		    Else
			If (idkey of dsid_preferred_corr  =
			    dbt_id of dbt_typ of ent_debit_set) Then
			    %^ Bypass, we have a circular reference, replacing
			    %^ same with same.
			    Move SPACES to Dsid_preferred_corr
			    %Beg  Dsid_preferred_corr = NULL ;  %End
			    Go to B200_GET_NEWPARTY_END
			end-if
		    end-if
	    end-if

	    Set Success_is in Dsid_didansi_ws to TRUE

            Evaluate dsid_pr_subtype_ws
	      When "SUB"
                 Set SUBSTITUTE in Dsid_pushdown to true

	      When "INS"
              When  SPACES
		 Set PUSHDOWN in Dsid_pushdown to true

	      When Other
		 %Beg
		 Dsid_compose ^OUT(Dsid_err_memo)
		           "Illegal submethod ", dsid_pr_subtype_ws, 
			   " in SI ", dsid_pr_ordinal_ws , " for ", 
			   Ent_debit_set.Dbt_typ, /;
%^	    	 Ftrscr.Debit.dbt_typ.dbt_id.Msg = "FTRSCR$_INV_AIN_SI";
		
			field_ws = "DEBIT.DBT_TYP.DBT_ID";
			Mnemonic_ws = "FTRSCR$_INV_AIN_SI";
		 %End
	         Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws

		 Perform X920_INFO_MEMO through X920_INFO_MEMO_END
		 Add 1 to Error_memo_count_ls
		 Set Failure_is in Debit_screen_account_status_ls to true
		 Set Success_is in Dsid_push_error to TRUE
		 Set Failure_is in Nothing_suspicious_ls to true
	    END-EVALUATE
	ELSE
	    Move SPACES to Dsid_preferred_corr
	    %Beg  Dsid_preferred_corr = NULL ;  %End
	END-IF.
B200_GET_NEWPARTY_END.

   EXIT.
%^
%^ Temp - give it a number later, just here for now.
%^
GET_AIN_PARAMS.
	%^
	%^ Only party here now, can't think what else DBT AIN would need
	%^
	Move 0 to Lcl_prms_remaining_ws.
	Perform with test after until Failure_is in Lcl_scan_Stat_ws
		Move spaces to Lcl_Param_name_ws
		Call "PRULE_MSG_READEX_PARM" using
			by reference Lcl_Param_name_ws
			by reference Lcl_Param_name_ws_length
			by reference Lcl_Param_type_ws
			by reference Lcl_prms_remaining_ws
			by reference Lcl_Param_value_ws
			by reference Lcl_Param_value_ws_length
			returning Lcl_scan_stat_ws
		If Success_is in Lcl_scan_stat_ws then
		    %^ All are alpha, copy use full fields
		    Evaluate Lcl_Param_name_ws
		    	when "MTS$PARTY"
				%^ Parse the party ID/ should be Idtype/Id
				%beg Dsid_parse ^in(Lcl_param_value_ws)
					dsid_preferred_corr.Idtype , "/" ,
					dsid_preferred_corr.Idkey,/;
				%end
				%^ Parse should be enough but...
				If Not (Success_is in dsid_parse_status) Then
			    	    %^ lets jut move it in, maybe the / is missing
				    Move Lcl_param_value_ws(1:Lcl_param_value_ws_length) to
				         dsid_preferred_corr%^ should be format
				    %Beg
					dsid_preferred_corr( .Idtype change,
			   			  		     .Idkey change ) ;
				    %End
				end-if
		    end-evaluate
		End-if
	End-perform.
%^
GET_AIN_PARAMS_END.
	EXIT.

B240_SET_ADDRESS.
* Sets actual rel address linkages in Message's debit party.
%^ Connect debit party address.
	%Beg
	BREAK: Ent_d_adr_set ;
	Relget_adr_set EQUATE: Ent_d_adr_set(NOMOD) ;  
	%End.

        Move Priority_flg of Ent_ftr_set to Dsid_hold_priority_ws.
	Call "SET_DEBIT_ADDRESS" USING
	    By reference Is_rptv_lookup_ls
	    By reference Nochange_bank_ls
	    By reference Dsid_loc_bank_change
	  RETURNING Dsid_ret_stat.

        If Priority_flg of Ent_ftr_set not = Dsid_hold_priority_ws
            %Beg
            Dsid_compose ^OUT(Dsid_info_memo)
                        "Priority set to ", Ent_ftr_set.flgs.priority_flg,
                        " by debit party ",Ent_debit_set.dbt_typ(.dbt_idtype,"/",.dbt_id) , / ;
            %End
            Perform X920_INFO_MEMO thru X920_INFO_MEMO_END
        End-if.

	If (Dsid_loc_bank_change NOT = 0 )
	    set success_is in Msg_bank_changed_ls to true
	END-IF

	If (success_is in Msg_bank_changed_ls)
	   AND (Success_is in Nochange_bank_ls)
	THEN
%^ Write an error memo indicating the discrepancy.
%^	    %Beg	    Ftrscr.debit.dbt_typ.dbt_idtype.msg = "FTRSCR$_NOXBNKDBTPRIV" ;	    %End
	    %Beg
		field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
		Mnemonic_ws = "VMSG$_NOXBNKDBTPRIV";
	    %End
	    Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
	    Add 1 to Error_memo_count_ls
	    Set Failure_is in Debit_screen_account_status_ls to true
	    GO TO B240_SET_ADDRESS_END
	END-IF.


* Completed address setup.
	%Beg
	BREAK: Dsid_acc_seq ;
	Ent_d_adr_set.account_seq CONN: Dsid_acc_seq(NOMOD) ;
	%End.
%^ Okay.  Now sanitize debit id.
	%Beg
	Dsid_parse ^IN(Ent_debit_set.dbt_typ.dbt_id), 
		^OPTION(Dsid_id_bank_ws, ":" ), Dsid_id_ws, ^SPACE, / ;
	%End.

	If (success_is in Msg_bank_changed_ls)
%^ Strip in the new bank so that the user can see it.
	    Move SPACES to Dbt_id of Dbt_typ of Ent_debit_set
	    %Beg
	    Dsid_compose ^OUT(Ent_debit_set.Dbt_typ.dbt_id ),
		Ent_debit_set.dbt_adr_bnk_id, ":", Dsid_id_ws, / ;
	    %End
	END-IF.

* Completed address setup.
	Set Success_is in Dsid_set_address to TRUE.

B240_SET_ADDRESS_END.



   EXIT.
B280_FIND_ACCOUNT.
%^  Calls REL_ACCOUNT_FROM_ADR to get an account from the current address.
%^  Ent_d_adr_set must be connected.  
	%Beg  
	BREAK: Ent_d_acc_set ; 
	BREAK: Ent_d_acc_rel_reg;
	 %End.
	IF (Dsid_know_acc_id NOT = SPACES )
	    If (Idbank of Dsid_know_acc_id NOT = SPACES )
		AND (Idbank of Dsid_know_acc_id NOT = 
					Dbt_adr_bnk_id of Ent_debit_set )
	        AND (Failure_is in Xbank_account_ok_ls )
	    THEN
%^ Debit party address and account cannot be in different banks.
		%Beg
%^	        Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOSUCHACC" ;
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
		field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
		Mnemonic_ws = "VMSG$_NOSUCHACC";
                %End
	        Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Set Failure_is in Debit_screen_account_status_ls to true
		Add 1 to Error_memo_count_ls
                GO TO B280_FIND_ACCOUNT_END
	    END-IF
	    Move 4 to Dsid_account_mode
            %Beg
	    Dsid_parse ^IN(Dsid_know_acc_id.idkey)
		^SPACE, Dsid_acc_id_arg, ^SPACE, / ;
	    %End				    
	ELSE
	    %^ We were not provided an account
	    %Beg
	    Dsid_next_status = Ent_d_adr_set State.CONN ;
	    BREAK: Relget_adr_set ;
	    %End
	    If (Dsid_next_status = 0 )
	       OR (Rel_id of Ent_d_adr_set = 0 )
	    THEN
%^ We have a NOF debit party.
                %Beg
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
                %End
	        GO TO B280_FIND_ACCOUNT_END
	    END-IF
	    %Beg
	  	Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD);
	    	Dsid_know_acc_id = NULL ;
	    	Dsid_acc_id_arg = NULL ;
	    %End
	    %^
	    %^ Check to see if this a "P" equivalent advice type
	    %^ OBSELETE    Call "GET_PID_ADV_DATA" using
	    %^   Returning Dsid_itsa_clearhouse

	    Set Failure_is in Dsid_itsa_clearhouse to True
	    %^
	    Perform B300_GET_ACC_FROM_CHAN thru
		    B300_GET_ACC_FROM_CHAN_END

	    If ( Success_is  in Dsid_got_channel_ws) and
	       (( clrhouse_is in Endpoint_type of Rchan_channel_set) or
	        ( rtgs_is     in Endpoint_type of Rchan_channel_set) or
	        ( endpoint_account of rchan_channel_set not = Spaces)) And
	     Not(Src_code of Ent_ftr_set = "FED" and   %^ 112177 	114318
	    	  tran_type of Ent_ftr_set = "DRW")     %^ Still get account for Drawdowns
	    Then
		If Dbt_idtype of dbt_typ of  ent_debit_set
		   Not = "D" AND "G" AND "F" AND "V" Then    
		    %^ Channel Determination provided an account number
		    %^ In Debitside_screen, we should ONLY be here for RPR reasons,
		    %^ Just let DO_ACCOUNT know that the account is already set.
		    Set Success_is in Dsid_itsa_clearhouse to True
		    Move 4 to Dsid_account_mode
		    %beg
		        dsid_know_acc_id = rchan_channel_set.endpoint_account;
		        Dsid_parse ^IN(Dsid_know_acc_id.idkey)
				^SPACE, Dsid_acc_id_arg, ^SPACE, / ;
		    %end
		end-if

	    ELSE %^ from rtgs channel

    	        If Account_type_ls NOT = SPACE
	            Move 0 to Dsid_account_mode
	        ELSE
	            Move 5 to Dsid_account_mode
	        END-IF
	        If (Dbt_idtype of Dbt_typ of Ent_debit_set =
					"D" OR "V" OR "G" OR "F" )
		    %Beg
		    Dsid_parse ^IN(Ent_debit_set.dbt_typ.dbt_id )
			^SPACE, Dsid_acc_id_arg, ^SPACE, / ;
		    Dsid_know_acc_id.Idtype = 
					Ent_debit_set.dbt_typ.dbt_idtype ;
	    	    %End
	    	END-IF
	   
		If (Dsid_acc_id_arg_length NOT = 0 ) And
	           (Dsid_acc_id_arg(1:Dsid_acc_id_arg_length) = SPACES )
	   	THEN
	                %^ Compensate for compose peculiarity
			%Beg  Dsid_acc_id_arg = NULL;  %End
		END-IF
	    end-if %^ from rtgs channel
	End-if.
	

        Set Failure_is in Dsid_ret_stat to true.

%^ 
%^  TODO - How do we handle this?
%^ 
%^        %^ Clear out the error message if we're about to try again
%^	%Beg  Dsid_temp1_vstr = Ftrscr.Debit.dbt_typ.dbt_idtype.Msg;  %End
%^	If Dsid_temp1_vstr not = SPACES then
%^           %Beg  Ftrscr.Debit.dbt_typ.dbt_idtype.Msg = NULL ;  %End
%^      END-IF


%^ No account type restriction.  Let's see if an account type was used
%^    to get the address.  If so, then let's try it.
	Move SPACE to Dsid_idtype_ws.
	If (Dbt_idtype of Dbt_typ of Ent_debit_set = "D" OR "G" OR "F" OR "V" )
	Then
	%^ 	Make the Account 'sticky' 		CR305
	%^
		    move 3 to dsid_account_mode
		    %Beg
		    Dsid_parse ^IN(Ent_debit_set.dbt_typ.dbt_id )
			^SPACE, Dsid_acc_id_arg, ^SPACE, / ;
		    Dsid_know_acc_id.Idtype = 
					Ent_debit_set.dbt_typ.dbt_idtype ;
	    	    %End
	END-IF.

	%^ Spr 95707
	If Account_type_ls Not = Spaces Then
		%^ we were givin an override, use it
		Move Account_type_ls to Dsid_idtype_ws
	end-if.


	%Beg  Dsid_next_function = Menu_next_function;  %End
	%Beg  Menu_arg_cmd_vstr = "";  %End

	If (dsid_acc_id_arg = Spaces) and
	   (dsid_idtype_ws = Spaces)
	Then
		%^		Mode 2  blank 	 Curr	Try for Default In curr
		%^	        Mode 0	Idtype D Curr	Try for Nostro
		%^		Mode 5  Get whatever
		%^
		Move Spaces to dsid_account_type_ws
		Move 2 to dsid_account_mode
		Call "REL_ACC_FROM_ADR" USING
       			By reference Dsid_account_mode
			By reference dsid_account_type_ws
			By reference Dsid_currency_ws
            		By reference Idtype of Dsid_know_acc_id
            		By reference Dsid_acc_id_arg
            		By reference Dsid_acc_id_arg_length
            		By reference Bnk_id of Ent_d_adr_set
          	RETURNING Dsid_ret_stat
		If Relget_reject_flag = "M"
	    	    %Exit Program
		End-if
		If dsid_next_function  not = Menu_next_function Then
			%^ Cancel/quitting 
			%Exit Program
		end-if
		If Dsid_next_function = Menu_next_function and
			Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    	    %Exit Program
		End-if
		If Success_is in Dsid_ret_stat Then
			Go To B280_FIND_ACCOUNT_CLEANUP
		end-if
		%^ No Default try for D
		Move "D" to dsid_account_type_ws
		Move  0  to dsid_account_mode
		Call "REL_ACC_FROM_ADR" USING
       			By reference Dsid_account_mode
			By reference dsid_account_type_ws
            		By reference Dsid_currency_ws
            		By reference Idtype of Dsid_know_acc_id
            		By reference Dsid_acc_id_arg
            		By reference Dsid_acc_id_arg_length
            		By reference Bnk_id of Ent_d_adr_set
          	RETURNING Dsid_ret_stat
		If Relget_reject_flag = "M"
	    	    %Exit Program
		End-if
		If dsid_next_function  not = Menu_next_function Then
			%^ Cancel/quitting 
			%Exit Program
		end-if
		If Dsid_next_function = Menu_next_function and
			Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    	    %Exit Program
		End-if
		If Success_is in Dsid_ret_stat Then
			Go To B280_FIND_ACCOUNT_CLEANUP
		end-if
		%^
		%^ No account, just try for 1 in currency or ambig screen
		%^
		Move space to dsid_account_type_ws
		%^	Move space to dsid_currency_ws
		Move 5 to dsid_account_mode
	End-if


	Call "REL_ACC_FROM_ADR" USING
            By reference Dsid_account_mode
            By reference dsid_idtype_ws
            By reference Dsid_currency_ws
            By reference Idtype of Dsid_know_acc_id
            By reference Dsid_acc_id_arg
            By reference Dsid_acc_id_arg_length
            By reference Bnk_id of Ent_d_adr_set
          RETURNING Dsid_ret_stat
	If Relget_reject_flag = "M"
	    %Exit Program
	End-if.
	If dsid_next_function  not = Menu_next_function Then
		%^ Cancel/quitting 
		%Exit Program
	end-if.
	If Dsid_next_function = Menu_next_function and
		Menu_arg_cmd_vstr = Idfunc of Menu_next_function
	    %Exit Program
	End-if.


B280_FIND_ACCOUNT_CLEANUP.

	If (Success_is in Dsid_ret_stat   )
	    If (Relget_return_bank NOT = SPACES )
		AND (Relget_return_bank NOT = 
					Dbt_adr_bnk_id of Ent_debit_set )
	        AND (Failure_is in Xbank_account_ok_ls )
	    THEN
%^ Debit party address and account cannot be in different banks.
		%Beg
%^	        Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOSUCHACC" ;
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
		field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
		Mnemonic_ws = "VMSG$_NOSUCHACC";
                %End
	        Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Set Failure_is in Debit_screen_account_status_ls to true
		Add 1 to Error_memo_count_ls
                GO TO B280_FIND_ACCOUNT_END
	    END-IF
	    %Beg
	    Ent_acc_set EQUATE: Ent_d_acc_set(NOMOD,
				REG:  Ent_d_acc_rel_reg (NOMOD));
	    BREAK: Ent_acc_set ;
	    Dsid_parse ^IN(Relget_return_key), ^SPACE,
			Ent_debit_set.Dbt_account.Idkey, ^SPACE, / ;
	    Ent_debit_set.Dbt_account (.Idbank = Relget_return_bank,
                                       .Idtype = Relget_return_idtype ) ;
	    dsid_know_acc_id = Ent_debit_set.dbt_account;
	    %End
	ELSE
            %Beg
            Ent_debit_set( .Dbt_account        = NULL,
                           .Dbt_acc_class      = NULL,
                           .Dbt_acc_prod_codes = NULL,
                           .Flgs.dbt_hold_flg  = NULL) ;
            %End
	    Set Failure_is in Account_okay_ls to true
	    If (Relget_msgcode NOT = 0 )
%^                %Beg Ftrscr.Debit.dbt_typ.dbt_idtype.Msg = Relget_msgcode ;  %End
		%beg
			field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
			Mnemonic_ws = RELGET_MSGCODE;
                %End
	      	call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws


	    END-IF
	END-IF.
	%Beg  BREAK: Relget_adr_set ;  %End.

B280_FIND_ACCOUNT_END.
   EXIT.


B300_GET_ACC_FROM_CHAN.
%^
%^	Load the Channel Account if we can
%^
%^
	Set Failure_is in dsid_itsa_clearhouse to True.
	Set failure_is in dsid_got_channel_ws to True.

	If (Src_code of Ent_ftr_set  = "RTS" and
	    incoming_msgtype of ent_ftr_set = "198")   OR
	   (Src_code of Ent_ftr_set = "FED" and   %^ 112177 	114318
	    	 (( tran_type of Ent_ftr_set = "DRW" or "NON") Or
 		   (tran_type of ent_ftr_set = "RTN" AND type_code of ent_ftr_set(3:2) = "33") Or
		   ((type_code of Ent_ftr_set = "1002" or "1007") and
		     Dbt_idtype of Dbt_typ of Ent_debit_set NOT = "A") ))  %^ 11239 get FED when A id left
	Then
		Move spaces to debit_account_ls	%^ clear what as called with
		Go to B300_GET_ACC_FROM_CHAN_END
	END-if.

	Set ID_IS in dsid_prchan_mode to True.
	If Dbt_adr_bnk_id of Ent_debit_set not = spaces then
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_debit_set.dbt_adr_bnk_id;
		%End
	Else
		%Beg
		dsid_chan_ident_ws.Idbank = Ent_ftr_set.Loc_info.Bank;
		%End
	End-if.

	%Beg
		dsid_chan_ident_ws.idkey = Ent_ftr_set.src_code;
	%End

	Call "GET_CHANNEL" using
			By Reference 	dsid_prchan_mode
			By Reference	dsid_chan_ident_ws
			By Reference 	dsid_chan_ident_ws_lengths
		returning dsid_got_channel_ws.

	If (Success_is in Dsid_got_channel_ws) and
	   ((clrhouse_is in Endpoint_type of Rchan_channel_set) Or
	    (RTGS_is in Endpoint_type of Rchan_channel_set))
	Then
		%^ If we have an 'on us' idtype, don't override with
		%^ network Account most likely a drawdown
		%^
		If  Dbt_idtype of dbt_typ of  ent_debit_set
			Not = "D" AND "G" AND "F" AND "V" Then    
		   If Endpoint_account of rchan_channel_set = Spaces Then
			Call "PRULE_SET_CHANNEL" using
				By Reference	dsid_chan_ident_ws
				By Reference 	dsid_chan_ident_ws_lengths
			returning dsid_pr_ret_stat
			%beg dsid_prm_values_remain = <0>; %end
			Call "PRULE_CHAN_GET_PARAM" using
				by reference Mts_channel_account_wc
				by reference Mts_channel_account_wc_length
				by reference dsid_prm_edit_type
				by reference dsid_prm_values_remain
				by reference dsid_prm_value
				by reference dsid_prm_value_length
			returning dsid_pr_ret_stat
			If Success_is in dsid_pr_ret_stat then
				%Beg
					dsid_parse ^in(dsid_prm_value)
			       			dsid_know_acc_id.Idtype, "/",
			       		^Oneof( (dsid_know_acc_id.Idbank, ":", dsid_know_acc_id.IdKey, /),
				       		(dsid_know_acc_id.IdKey, /) );
				%End
				If Success_is in dsid_parse_status then
					Move idkey of dsid_know_acc_id to debit_account_ls
				End-if
		        End-if
			Call "PRULE_BREAK_CHANNEL" returning dsid_pr_ret_stat
		   else
			%^ Channel Determination provided an account number
			%^ In Debitside_screen, we should ONLY be here for RPR reasons,
			%^ Just let DO_ACCOUNT know that the account is already set.
			Set Success_is in Dsid_itsa_clearhouse to True
			%beg
			   dsid_know_acc_id = rchan_channel_set.endpoint_account;
			%end
			Move Idkey of Dsid_know_acc_id to Debit_account_ls
			%^ We have an account now, if Debit is ovr "?", clear back to
			%^ "*" if on AUX
			If dbt_ovr of dbt_typ of Ent_debit_set = "?" Or tran_type of Ent_ftr_set = "DDR"
			Then	%^ we were ambiguous regarding Account, fix that
				If dbt_adr_ptr_ok of flgs3 of ent_debit_set = "T"
				then
					If dbt_rel_id of ent_debit_set Not = 0
					Then
						%beg ent_debit_set.dbt_typ.dbt_ovr = Null; %end
					Else	%^ AUX hit
						%beg ent_debit_set.dbt_typ.dbt_ovr = "*";  %end
					End-if
				end-if
			End-if
		   End-if
		end-if
	end-if.

B300_GET_ACC_FROM_CHAN_END.
	EXIT.

B310_CONN_ACCOUNT.
%^ Finds actual debit account set, hooks it up, and copies necessary info.
	%Beg  Dsid_conn_ws = Ent_d_acc_set State.conn ;  %End
	If (Dsid_conn_ws = 0 )
	    %ace_conn_root_q Rel_acc_index ;
	    %Beg
	    SEARCH: Rel_acc_index (FORWARD, GEQ, .Rel_name_key 
				(.Idbank        = Dsid_know_acc_id.Idbank,
				 .Idtype        = Dsid_know_acc_id.Idtype,
				 .Idkey (.Idacc = Dsid_know_acc_id.Idkey,
				         .Idadr = NULL,
			       		 .Idpad = NULL ) ) ) ;
            %End
	    If (Failure_is in Rel_acc_index_status   )
	       OR (Idbank of Rel_name_key of Rel_acc_index NOT =
						Idbank of Dsid_know_acc_id )
	       OR (Idtype of Rel_name_key of Rel_acc_index NOT =
						Idtype of Dsid_know_acc_id )
	       OR (Idacc of Idkey of Rel_name_key of Rel_acc_index NOT =
						Idkey of Dsid_know_acc_id )
	    THEN
		%Beg
%^	        Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOSUCHACC" ;
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
		field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
		Mnemonic_ws = "VMSG$_NOSUCHACC";
                %End
	        Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Set Failure_is in Debit_screen_account_status_ls to true
		Add 1 to Error_memo_count_ls
	        %Beg  	Rel_acc_index (EQL) ;  %End
                GO TO B310_CONN_ACCOUNT_END
	    END-IF
	    %Beg
	    BREAK: Ent_d_acc_rel_reg;
  	    Rel_acc_index CONN: Ent_d_acc_set(NOMOD,
				REG:  Ent_d_acc_rel_reg (NOMOD));  
	    %End
	    If (Failure_is in Rel_acc_index_status   )
		OR (Failure_is in Ent_d_acc_set_status   )
	    THEN
		%Beg
%^	        Ftrscr.debit.dbt_typ.dbt_idtype.msg = "VMSG$_NOSUCHACC" ;
                Ent_debit_set( .Dbt_account        = NULL,
                               .Dbt_acc_class      = NULL,
                               .Dbt_acc_prod_codes = NULL,
                               .Flgs.dbt_hold_flg  = NULL) ;
		field_ws = "DEBIT.DBT_TYP.DBT_IDTYPE";
		Mnemonic_ws = "VMSG$_NOSUCHACC";
                %End
	        Call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws
		Set Failure_is in Debit_screen_account_status_ls to true
		Add 1 to Error_memo_count_ls
	        %Beg  	Rel_acc_index (EQL) ;  %End
                GO TO B310_CONN_ACCOUNT_END
	    END-IF
        END-IF.
	Perform B320_SET_ACCOUNT thru B320_SET_ACCOUNT_END.
	Move Currency_id of Ent_d_acc_set to Currency_found_ls
	If (Currency_found_ls = SPACES)
	THEN
	    %Beg
	    BREAK: Dsid_bnk_union ;
	    SEARCH: Bnk_index 
			(Key = Ent_debit_set.Dbt_adr_bnk_id ) ;
	    %End
	    If Failure_is in Bnk_index_status  
		Move SPACES to Currency_found_ls
	    ELSE
		%Beg  Bnk_index CONN: Dsid_bnk_union (NOMOD) ;  %End
		Move Base_currency_id of Dsid_bnk_union to Currency_found_ls
	    END-IF
	END-IF.
	%Beg  	Rel_acc_index (EQL) ;  %End.

B310_CONN_ACCOUNT_END.
   EXIT.


B320_SET_ACCOUNT.
	Call "SET_DEBIT_ACCOUNT" USING
	    By reference Debit_account_ls
	    By reference Is_rptv_lookup_ls
	    By reference Special_fee_key_ls
	  RETURNING Dsid_ret_stat.

* Completed account setup.
	Set Success_is in Dsid_set_account to TRUE.
	Set Success_is in Account_okay_ls to true.

B320_SET_ACCOUNT_END.


   EXIT.
C440_COPY_ADDRESS.
%^
	If (Adr_name_length of Relget_adr_set_lengths = ZERO )
	   AND (Dbt_name1_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name1 = NULL;  %End
	END-IF.
	If (Adr1_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name2_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name2 = NULL;  %End
	END-IF.
	If (Adr2_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name3_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name3 = NULL;  %End
	END-IF.
	If (Adr3_length of Relget_adr_set_lengths = ZERO )
	    AND (Dbt_name4_length of Ent_debit_set_lengths NOT = ZERO )
	THEN
 	    %Beg  Ent_debit_set.Dbt_name4 = NULL;  %End
	END-IF.
%^
	%Beg
	Dsid_Compose Relget_adr_set (
		.Adr_name (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name1), ^_, /)),
		.Adr1 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name2), ^_, /)),
		.Adr2 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name3), ^_, /)),
		.Adr3 (^IF_NOTNULL(^OUT(Ent_debit_set.Dbt_name4), ^_, /))) ;
%^ Build a DBT_ADR_TYPE field from country code and 
%^   ADR_TYPE field of Relget_adr_set
	Dsid_Compose ^OUT(Ent_debit_set.Dbt_adr_type),
		Relget_adr_set.Country_code(^STRING<2>),
		Relget_adr_set.Adr_type, /;
	%End.

%^ copy in the zip code too
	If (Zip of Relget_adr_set NOT = SPACES )
	    Call "ZIPSUB" Using
		by reference Dbt_name4 of Ent_debit_set
		by reference Dbt_name4_length of Ent_debit_set_lengths
		by reference Dbt_name3 of Ent_debit_set
		by reference Dbt_name3_length of Ent_debit_set_lengths
		by reference Zip of Relget_adr_set
		by reference Line_flg_ws

	    EVALUATE Line_flg_ws
		WHEN "4"
		    %Beg  Ent_debit_set.Dbt_name4 CHANGE;  %End

		WHEN "3"
		    %Beg  Ent_debit_set.Dbt_name3 CHANGE;  %End

	    END-EVALUATE
	END-IF.
C440_COPY_ADDRESS_END.



   EXIT.
C450_PUSH_DEBIT.

* Checks to make sure that there's an available creditside slot.
* Then calls PUSH_DEBIT_PARTY to push debit party down.

	If (obk_id of Obk of Ent_debit_set NOT = SPACES )
	    %^  No place to push into.
	    Set Failure_is in Dsid_ret_stat to true
	ELSE
	    CALL "PUSH_DEBIT_PARTY" USING
		By reference Bank of Loc_info of Ent_ftr_set
		By reference Dsid_pushed_debit
	      RETURNING Dsid_ret_stat
	END-IF.

	If Failure_is in Dsid_ret_stat   then
	    %^  No place to push into.
            %Beg
            Dsid_compose ^OUT(Dsid_info_memo)
                "No debitside slots for preferred corr ",
                    Dsid_preferred_corr, / ;
           %^	 Ftrscr.Debit.dbt_typ.dbt_id.msg = "VMSG$_NOMORE_PARTIES";
		field_ws = "DEBIT.DBT_TYP.DBT_ID";
		Mnemonic_ws = "VMSG$_NOMORE_PARTIES" ;
            %End
	     call "FTRSCR_WRITE_ERROR" using
			by reference field_ws
			by reference Mnemonic_ws


            Perform X920_INFO_MEMO through X920_INFO_MEMO_END
            Set Failure_is in Debit_screen_account_status_ls to true
%^            Move 1 to Dsid_push_error
	    Set Success_is in Dsid_push_error to True
            Set Failure_is in Nothing_suspicious_ls to true
            %^ Reconnect the current Ent_d_adr_set if necessary
            If Dbt_rel_id of Ent_debit_set not = 0
                %Beg Dsid_conn_stat = Ent_d_adr_set State.conn ; %End
                If (Dsid_conn_stat = 0 )
                THEN
                    %Beg
                    Ent_debit_set.Dbt_adr_set_ptr CONN: Ent_d_adr_set(NOMOD) ;
		    Dsid_conn_stat = <1>;
                    %End
                END-IF
                %Beg Dsid_conn_ws = Relget_adr_set State.conn ; %End
                If (Dsid_conn_ws = 0 ) or
		   (Rel_id of Relget_adr_set not = Rel_id of Ent_d_adr_set)
                THEN
                    %Beg
        	    BREAK: Relget_adr_set;
        	    Ent_d_adr_set EQUATE: Relget_adr_set(NOMOD) ;
		    Dsid_conn_ws = <1>;
                    %End
                END-IF
	      ELSE
		%Beg
		BREAK: Ent_d_adr_set;
		BREAK: Relget_adr_set;
		%End
		Move zeroes to Rel_id of Ent_d_adr_set
		Move zeroes to Rel_id of Relget_adr_set
		Move zeroes to Dsid_conn_stat
		Move zeroes to Dsid_conn_ws
	    END-IF

	END-IF.

C450_PUSH_DEBIT_END.


* Utility paragraphs.

   EXIT.
X900_ERROR_MEMO.
*  This paragraph writes an error memo using the text string in Dsid_err_memo.
	If Dsid_err_memo_length = 0 
	    GO TO X900_ERROR_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_err_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_err_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_err_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)
	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
	ADD 1 TO Error_memo_count_ls.
X900_ERROR_MEMO_END.


   EXIT.
X920_INFO_MEMO.
*  This paragraph writes an informational trace memo using the text string
*      in Dsid_info_memo.
	If Dsid_info_memo_length = 0 
	    GO TO X920_INFO_MEMO_END
	END-IF.
	Move SPACES to Dsid_temp_memo
	If Dsid_info_memo_length > 80
	    Move 80 to Dsid_temp_memo_length
	ELSE
	    Move Dsid_info_memo_length to Dsid_temp_memo_length
	END-IF
	Move Dsid_info_memo(1:Dsid_temp_memo_length) to 
		Dsid_temp_memo(1:Dsid_temp_memo_length)

	%Beg
	ALLOC_END: Ent_msg_history (
			.qname (
				.Idbank = Ent_ftr_set.Loc_info.Bank,
				.Idloc = NULL,
				.Idname= "*SYS_MEMO"),
			.memo   = Dsid_temp_memo,
			.qtype	= "OBJTYP$_NULL");
	%End.
X920_INFO_MEMO_END.
	EXIT.

X930_CHANGE_DEBIT.
	%^
	%ACE_IS Relget_adr_set connected giving Dsid_conn_stat;
	%^
	%Beg
	BREAK: Prulepty_rule_seq(NOMOD);
	BREAK: Prulepty_party_adr_set(NOMOD);
	%End
	If Success_is in Dsid_conn_stat Then
		%beg Prulepty_source rel_is;
		     Prulepty_party_adr_ok Success_is;
	     	     Relget_adr_set.adr_proc_rule CONN:
						      Prulepty_rule_seq(NOMOD);
		     Relget_adr_set EQUATE: Prulepty_party_adr_set(NOMOD);
		%end
		If (Rel_id of Relget_adr_set = 0 )
		    %Beg  Prulepty_source aux_is;  %End
		end-if
	Else
		%beg Prulepty_source host_is;
		     Ent_Debit_set.Dbt_proc_rule CONN:
						       Prulepty_rule_seq(NOMOD);
		     BREAK: tmp_party_seq;
		     Prulepty_party_adr_ok Failure_is;
		%end
		Initialize Prulepty_party_adr_set
	end-if.

	%beg
	    Dsid_id_bank_ws = Ent_debit_set.dbt_adr_bnk_id;
	    Prulepty_bank_prof_rec = NULL;
	%end
	If (Dsid_id_bank_ws = SPACES)
	    %Beg  Dsid_id_bank_ws = Ent_ftr_set.Loc_info.Bank;  %End
	end-if

	If (Dsid_id_bank_ws = Bnk_id of Menu_bnk_union)
		%beg
		     Prulepty_bank_prof_rec =
					     Menu_bnk_union.Bnk_profile_id_rec;
		%end
	else
		%Beg
		   BREAK: Dsid_bnk_union ;
		   SEARCH: Bnk_index 
			   (Key = Dsid_id_bank_ws );
		%end
 		If (Success_is in Bnk_index_status)
			%Beg Bnk_index CONN: Dsid_bnk_union(NOMOD); %end
		end-if
		%beg
  		   Prulepty_bank_prof_rec = dsid_bnk_union.Bnk_profile_id_rec;
		%End
	end-if.

	Call "PRULE_CHANGE_DEBIT" returning Dsid_ret_stat.

X930_CHANGE_DEBIT_END.
	EXIT.

%^******************************************************************************
%^
%^      DEBIT_REPET_STATE
%^
%^******************************************************************************
%^
%^ Utility routine to return a Debit state argument suitable for cleaning up
%^ after an instance of a repetitive has been created.  (DEBIT_LOOK_ACCOUNT or
%^ DEBIT_SCREEN_ACCOUNT would then be called with this state argument.)  This
%^ routine cohabitates with the DEBITSIDE_<mumble> and DEBIT_<mumble>_ACCOUNT
%^ routines so the details of the state information used by the 
%^ DEBIT_<mumble>_ACCOUNT routines do not need to be public.  Hiding this makes
%^ it possible to change it as necessary without changing all of the callers.
%^
%^   Calling format:
%^	Call "Debit_repet_state" 
%^	    Returning Debit_state_long
%^
%^ Modification history:
%^
%^******************************************************************************
%module DEBIT_REPET_STATE;

%^*****************************************************************************
%^ Macro definitions.
%^ %MAR
%^ .default	displacement,long
%^ %end
%^*****************************************************************************

%linkage
01  Dsid_Intern_long_ls 	  	%long;

%Procedure RETURNING Dsid_intern_long_ls.

%^*****************************************************************************

A100_MAIN.
	Set DSTATE_INIT in Dsid_internal to TRUE.
	%Beg  Dsid_intern_word = Dsid_internal ;  %End.
	Move Dsid_intern_word to Dsid_intern_long_ls.

A100_MAIN_END.  
	%EXIT PROGRAM.
