%module ACCT_LOOKUP;
***************************************************************
*                                                             *
* Copyright 1998-2009 by IntraNet, Inc. All rights reserved.  *
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
%^
%^	This module has the misnamed address lookup routine used by the input
%^	mapping programs
%^		ACCT_LOOKUP
%^
%^
%^ Calling conventions:
%^	I	Ambig_suppress_ls:	str(1);	  %^ If "T" then partial
%^			matches are not permitted (or something like that).
%^	I	Search_all_banks_ls:	str(1);	  %^ If "Y" then search is
%^			across banks.
%^	I	Search_for_currency_ls:	str(3);	  %^ If not "   " then used
%^			to resolve ambiguous lookups.
%^	I & O	Fail_adr_pend_del_ls:	str(1);   %^ Set to F to enable
%^					detecting an address pending deletion;
%^					Returned as "T" when passed in as
%^					     "F" and address IS pending delete.
%^
%^
%^	Memory and Static domains SEARCH'ed.
%^	Static_root CONN'ected.
%^	Menu_bnk_union CONN'ected.
%^
%^
%^ Change history:
%^	A. Smith	30-APR-1992 06:52:04.79
%^	Copy FED short name from AUX DB if present; don't copy null fields
%^	from adr set to message.
%^	Propgate idtype when moving down to BNP in preferred correspondent case.
%^
%^	D. Beer		11-SEP-1992
%^	When a SWIFT Id in the credit party is not on file, try using the
%^	AUX database SWIFT-to-ABA cross-reference index to determine an ABA
%^	number.
%^
%^	A. Smith	26-NOV-1993 18:17:49.68
%^	Add support for MAP_HO_INSERTION config flag.
%^	DEBIT_LOOKUP
%^	When doing a debit lookup by SWIFT identifier, then if the lookup
%^	fails and the key is 11 characters, try again with the 1st 8 characters
%^	of the key.  If this lookup succeeds, and the SBK field is not filled,
%^	move the 11 character key to the SBK IDTYPE/ID, and make the
%^	8-character key the debit party. The transfer will now succeed if the
%^	sending branch is set up with debit authority over the account of the
%^	branch with the 8 character key (presumably the head office).
%^
%^	CREDIT_LOOKUP
%^	If the credit party has an 11 character identifier, and the identifier
%^	is not found in the rel file but the 8 character identifier is, then
%^	the 8 character identifier is treated as an implied preferred
%^	correspondent.
%^
%^	Fred M. Hanna	13-JAN-1994 10:41:41.18
%^	Add support for idtype "I" lookups.  Since idtype "I" is an interbank
%^	id the Search_all_bank_ls argument is ignored and the serach is always
%^	across all banks.  The Ambig_suppress_ls is with reguard to all banks
%^	as well.  Fix bugs introduced by previous enhancments.
%^
%^      A. Caci         21-JUN-1994
%^      Added code to support the new standing instructions structure and credit/pymnt
%^	structure.
%^
%^      Z. Los          09-Aug-1994
%^      Added code to support REL index lookup in acct_lookup
%^
%^	Z. Los  19-Oct-1994 15:00
%^	COPY_CDT_DATA: spr 0693
%^	COPY_DBT_DATA: spr 0693
%^	The problem is in a000_cdt_data. When the credit or debit Id on the
%^	rel file only contains 3 lines, however there was four lines already
%^	mapped to the credit part name and address. This cause a mixup in the
%^	credit party mapping.
%^
%^	Fred M. Hanna	 4-NOV-1994 13:25:13.45
%^	Fix bug where a bad bank caused Acct_bnk_union to be disconnected
%^	without clearing the Id field used to control the local subject
%^	caching.  Thus the next lookup to a good bank would fail when the
%^	good bank was the same as the last connected bank.
%^	Finally fix the existing idtype "R" (rel_index) lookup bugs (dating
%^	from 09-Aug-1994).
%^
%^	Fred P. Isaacs   31-MAR-1995
%^	Added equated second lookup sequence so search for duplicate can be
%^	done in it without moving search index off hit.
%^
%^	Fred P. Isaacs   18-MAY-1995
%^	Added NEXT_ACCT_LOOKUP routine.
%^	Added UNCROSS_LOOKUP routine.
%^
%^	Fred P. Isaacs	 27-JUL-1995
%^	Added answerback parsing code for CLY PER 3520
%^
%^	H. Podany	13-Apr-1995
%^	In ACCT_LOOKUP (paragraph X100_FIND_FIRST_HIT) Telex answerback lookup
%^	ids must match the on-file answerback index key exactly, spr# 3586.
%^
%^	Fred P. Isaacs  8-Nov-1995
%^	Eliminated obsolete credit and debit lookup routines.  CREDITSIDE_
%^	and DEBITSIDE_routines should be used instead.
%^
%^	Fred P. Isaacs	 29-NOV-1995
%^	Set up unused fields a bit more cleanly for AUX Swf_to_aba_index.
%^
%^	Fred P. Isaacs	 12-JAN-1996
%^	Removed UNCROSS_LOOKUP routine -- no longer needed and potentially
%^	dangerous.  Fixed XREF error exit in ACCOUNT_LOOKUP.  Made
%^	ACCT_LOOKUP set Relget_return_key, Relget_return_idtype, and
%^	Relget_return_bank on return.
%^	
%^	Fred P. Isaacs  1-FEB-1996
%^	Fixed I and K lookups to blank out the bank field.
%^	Fixed bug in answerback hit checking.
%^
%^	Fred P. Isaacs	 20-FEB-1996
%^	Now eats trailing slashes on address ids.
%^
%^	Fred P. Isaacs	 22-MAR-1996
%^	Now properly fills out Relget_return variables for aux db hits.
%^
%^	Fred P. Isaacs	  9-APR-1996
%^	Added $ mode for aux-only BIC tape lookup.
%^	Added s mode for aux-only SWF key to UID tape lookup.
%^	Added duplicate checking for AUX lookups.
%^	Made search for 8-char tid try for both "SWIFUS33   " and "SWIFUS33XXX"
%^
%^	Fred P. Isaacs	   9-MAY-1996
%^      Made search mode explicit for each search since we now mix EQL
%^	and GEQ.
%^
%^	Fred P. Isaacs	  12-JUN-1996
%^      Cleaned up parsing for account numbers to remove occasional
%^	mis-searches caused by leftover stray characters.
%^      Made it return error for blank name lookup.
%^
%^      John R. Phelan    04-SEP-1996
%^      Deleted superfluous test of Ambig_suppress_ls flag. #19851
%^
%^      Fred P. Isaacs    22-NOV-1996
%^      Added PARTIAL_MATCH_AMBIG INTRTL flag to cause mismatchs which would
%^      have succeeded as partial key matches to be flagged with a
%^      "?" ambiguity indicator.  #20437.  Migrated to 4.1 by John R. Phelan.
%^
%^      Fred P. Isaacs    22-NOV-1996
%^      Now sets Relget_msgcode to vmsg$_ambig_lookup for a "?" indicator if
%^      the lookup really was ambiguous and sets relget_msgcode to
%^      vmsg$_part_lookup if the "?" indicates a partial match only.  #21605.
%^	Migrated to 4.1 by John R. Phelan
%^
%^      John R. Phelan    16-JAN-1997
%^      Check for ambiguous swift id's in the X400_SWIFT_SEARCH paragraph
%^      if Ambig_suppress_ls is not "T".  #24381
%^
%^      John R. Phelan    19-MAY-1997
%^      Turn on disambiguation of addresses based on currency code even in
%^      the baseline environment.  #29039
%^
%^      John R. Phelan    04-JUN-1997
%^      Allow lookup of Dbt Party ABA in AUX file if source is FED.  #29405
%^
%^      Tom Carroll       12-JUN-1997
%^      Allow id type of "a" to force an AUX lookup using the ABA number.
%^      SPR #28336.
%^
%^	A. Smith	28-Jun-1997		SPR #27131
%^	Use "P_ID" parameters in the debit party bank-union to replace
%^	hard-coded references to "CHP" as the source-code associated with the
%^	"P" idtype.
%^
%^	F. Kelley	09-Jul-1997		SPR #27127
%^	Add "E" idtype to lookup.                                              
%^
%^	F. Kelley	26-Jan-1998		SPR #30746
%^	Allow idtype of "e" for lookup to Aux_ext_id_index.
%^	Migrated to V4.3 by Jphelan.
%^
%^      John R. Phelan  03-FEB-1998		SPR #35648
%^      Changed to call the new Lookup_pid_rec subroutine to lookup
%^      up "P" ID information, since this can vary depending on bank,
%^      account suffix, currency, etc...  This change is necessary for 
%^	the Amex German Clearing Interface.
%^
%^      John R. Phelan  10-Feb-1998   		SPR #39285
%^      Add simple processing for Aux AIN's.  This is being implemented
%^      for Amex German Clearing.  If an Aux address set has a standing
%^      instruction, it is returned.
%^
%^	Fred P. Isaacs	11-Mar-1998		 40423
%^	Make sure that ambiguous matches are flagged; make sure that ID/IDTYPE
%^	found is always returned as Relget_return_idtype/Relget_return_key
%^
%^	Fred P. Isaacs	18-Mar-1998		 40701
%^      Partial match ambiguity was being checked in an overly aggressive
%^	fashion.
%^
%^	Fred Isaacs	21-Apr-1999		53240
%^	Now does proper seq connection and TOP to get to AUX CIF account.
%^
%^	Tom C. Carroll	12-Feb-1999		 50463
%^      Implement searching on the "Me_First_Index" which keyed on       
%^	id type and id only.  This allows us to be able to detect an
%^	ambiguous situation between banks.   
%^
%^	John R. Phelan	 1-Mar-1999		 51896
%^	Fix to "Me_first_index" logic.  There was a mixed and/or condition
%^	which was causing the Me_first index to be searched even
%^	when R_ADR_ACC_ADR_INDEX was set to "no".
%^
%^      Tom Carroll     23-Mar-1999              52583
%^      Change the AUX lookup routine to CONNect to the relget_adr_set with
%^      NOMOD.  AUX lookups were trapping with object-locked.
%^
%^	Fred Isaacs	21-Apr-1999		53240
%^	Now does proper seq connection and TOP to get to AUX CIF account.
%^
%^	Don Ryan	07-May-1999		#15526 & #54097
%^	Added a new argument (Acct_look_pend_del_ls).  If the argument is set to
%^	"F" and the Pend_del_date of Relget_adr_set is greater than zero, the
%^	error message (address is marked for deletion) is displayed.
%^
%^	J. Walsh	21-Jun-1999		55379
%^	Correct address is marked for deletion logic.
%^
%^	J. Walsh	02-Nov-2001	SPR 79388
%^	If address is marked for deletion, check AUX.
%^
%^	J. Walsh	25-Nov-2001	SPR 79388
%^	Backout logic: If address is marked for deletion, check AUX.
%^
%^***************************************************************************
%^ Revisions to ported version:
%^	Fred P. Isaacs  28-APR-1998	42027  (VAX product 29405)
%^	Allow lookup of debit party ABA in AUX file if source is FED.
%^
%^	Fred P. Isaacs	5-Oct-1999	58119 for port
%^	Un-suffixed PID may be used as address selector.  Will not try for
%^	duplicate, since that would likely find same address.  Will do partial
%^	match to get address but will not indicate it.
%^
%^	Fred P. Isaacs	14-Oct-1999	58119 for port
%^	Special match to numeric-only part of P ID is allowed; if done, disables
%^	duplicate check (since other suffixes on same P number would be dupes.)
%^
%^	H. Podany	23-Mar-2000	Spr# 59856
%^	Make changes to support java translator.  Remove multiple go to' in a
%^	paragraph.
%^
%^	Fred P. Isaacs	14-NOV-2000	66265
%^	Make Y (trade account) idtype useable as an address key.  Will accept
%^	Y without suffix as unambiguous key.
%^
%^	Fred P. Isaacs	2-OCT-2001	77459
%^	The "me_first" "L" index can be populated selectively.  If we
%^	fail on an "L" lookup, we should try searching the appropriate
%^	account address index with the original key.
%^
%^	Chris Fujiyoshi 16-APR-2002
%^	Add new Acct_que_type_ws value "E" for extended_id to support
%^	supplemental id sequence.
%^
%^	Diana Pacelle	23-May-2002
%^	Account number expansion.
%^
%^	Ken Bjelke	19-Aug-2002 	Spr 88085
%^	Change the usage for P accounts. They are now in the adr_set as
%^	opposed to being in an index. Change the prtocessing to account_type "R"
%^	instead of "A".
%^
%^
%^	Ken Bjelke 	23-Aug-2002
%^	Add Prule provisions for CDTAIN
%^
%^	Ken Bjelke	22-Nov-2002
%^	Remove CDTAIN stuff. Leaving the AUX relget_adr_set connected
%^	will take care of that back in Lookups.
%^
%^	Ken Bjelke 	26-Sep-2003	104780
%^	No longer skip AUX searches for BIC's in the DBT and CDT parties.
%^	
%^	J. Walsh	12-Nov-2003	106729
%^	REL 8 vs. 11 character BICs.
%^
%^	C. Crain 	15-Dec-2003	106192
%^	Allow "E" id searches to look at AUX if not found on REL.
%^
%^	J. Walsh	16-Mar-2004	110308
%^	Take pending deletion addresses into consideration when search all banks
%^	is used.
%^
%^	D. Ryan		29-Sep-2004	116715
%^	Correct a "debit party lookup failure" in MRE for an ambiguous account
%^	in paragraph X200_check_ambiguous.  Specifically for Branches (idtype).
%^
%^	Joanne Curley	15-Jul-2005	120610 (migrate)
%^	When ID on AUX but not REL and not a duplicate - use Subject to set
%^	value of Lookup_return_key_ws (so length will be set).  Lookup_return_key_ws
%^	is returned to the caller in Relget_return_key fsect value.
%^
%^	Ken Bjelke	19-Aug-2005	124825
%^	Pending Delete account not being detected. 
%^
%^	J. Walsh	22-Nov-2005	127281
%^	Commented out BREAK: Acct_lookup_index in B215_AUXILIARY_RESULTS
%^	because of SBJNOTCON trap.
%^
%^	J. Walsh	08-Feb-2006	128440
%^	Set the acct_lkup_pend_del_ls flag to T when address is pending deletion.
%^
%^	J. Walsh	08-Nov-2006	132667
%^	Correct lookup of P id.
%^
%^ 	Ken Bjelke 	17-May-2007	137551
%^	Allow AUX lookup for A type, Ddebit party when SEC src and ANT type. 
%^
%^	Ken Bjelke 	19-May-2008	CR2779	
%^	Remove "P" from the eligable list for "L" type lookups. P is
%^	no longer an Accounting Idtype. Was resulting in a strange loop.
%^
%^	Ken Bjelke CR 4209
%^	 	Ken Bjelke 	30-Aug-2006	132811
%^		Check on AUX for item found Pending Delete on REL. 
%^
%^	J. Walsh	19-Jun-2009	CR11857
%^	Disambiguate me-first lookup.
%^
%^	J. Walsh	13-Jul-2009	CR12410
%^	Correct the logic in the disambiguation of the me-first lookup.
%^
%^	J. Walsh	27-Jul-2009	CR11122
%^	Improve error handling around REL ID lookup.
%^
%^ End Revision History
%^****************************************************************************
 
%^ Macro definitions.
 
 
%^-deleted macro code %mar %^TRdel
%^-deleted macro code .default	displacement,long %^TRdel
%^-deleted macro code %end %^TRdel
%^-deleted macro code  %^TRdel
%^-deleted macro code  %^TRdel

%^****************************************************************************
 
%^ Subject definitions.
 
 
%def <ENTFTR>	 %`SBJ_DD_PATH:ENTFTR_FSECT.DDL`	%end %^  TRmod
%def <ENTREPT>	 %`SBJ_DD_PATH:ENTREPT_FSECT.DDL`	%end %^  TRmod
%def <ENT> 	 %`SBJ_DD_PATH:ENT_FSECT.DDL`		%end %^  TRmod
%def <ACE>	 %`SBJ_DD_PATH:ACE_FSECT.DDL`		%end %^  TRmod
%def <RELGET>	 %`SBJ_DD_PATH:RELGET_FSECT.DDL`	%end %^  TRmod
 
%def		<LKUP_SUBS_WS>		%^ local fsect
 
 

%^****************************************************************************
 
%^ Working storage subject definitions.
 
 
Acct_lookup_index:	que(	%`SBJ_DD_PATH:REL_NAME_INDEX.DDF`); %^TRmod
Grp_lookup_index:	que(	%`SBJ_DD_PATH:REL_NAME_INDEX.DDF`); %^TRmod
Temp_lookup_index:	que(	%`SBJ_DD_PATH:REL_NAME_INDEX.DDF`);
 
Aux_db_set:		set(	%`SBJ_DD_PATH:AUX_DB_SET.DDF`); %^TRmod
 
Lcl_adr_acc_seq:	seq(	%`SBJ_DD_PATH:REL_NAME_SEQ.DDF`)  %^TRmod
				Scan_Key = Rel_name_key.Idtype ; %^ TRmod
Temp_rel_union:		set(	%`SBJ_DD_PATH:REL_UNION.DDF`); %^TRmod
Temp_null_set:		set(	%`SBJ_DD_PATH:DAT_NULL_SET.DDF`) ; %^TRmo2
Temp_null_seq:		seq(	%`SBJ_DD_PATH:DAT_NULL_SEQ.DDF`) ; %^TRmo2
Temp_name_key:		rec( 	%`SBJ_DD_PATH:ADR_ID_REC.DDF` );  %^TRmod


%^****************************************************************************
 
%^ Working storage program control flags.
   
 
Ent_adv_seq_conn_ws:	long;
Bank_specified_wf:	Boolean;
Interbank_search_wf:	Boolean;
 
Call_status_wf:		Boolean;	%^ Status of last COBOL call.
Subject_status_wf:	Boolean;	%^ Saved subject status.
Duplicate_checked_wf:	Boolean;	%^ Checked for duplicate
Duplicate_found_wf:	Boolean;	%^ found a duplicate
Swift_HO_search_wf:	Boolean;	%^ Doing an 8-char SWIFT TID or its
					%^ equivalent SPACE or XXX forms.
Aux_mode_wf:		Boolean;	%^ Aux mode search; don't do TOP:
Twiddle_idle_ws:	long ;		%^ Does nothing.
Aux_mode_conn_ws:	long;		%^ Check connections

%^****************************************************************************
 
%^ Working storage constants.

Vmsg_dat_notonfile_wc:		str = "VMSG$_DAT_NOTONFILE"; %^  TRmod
Vmsg_lookupfail_wc:		str = "VMSG$_LOOKUPFAIL"; %^  TRmod
Vmsg_ambig_lookup_wc:           str = "VMSG$_AMBIG_LOOKUP"; %^  TRmod
Vmsg_part_lookup_wc:            str = "VMSG$_PART_LOOKUP"; %^  TRmod
Vmsg_adr_pend_del_wc:		str = "VMSG$_ADR_PEND_DEL"; 

%^****************************************************************************
 
%^ Working storage variables.
 
Tran_lkup:		transaction; 
Insert_party_flag_ws:	Boolean;
Override_flg_ws:	str(1);
 
Lookup_search_idtype_ws: str(1);
Lookup_return_idtype_ws: str(1);
Lookup_return_key_ws:	vstr(64);
 
Temp_acc_id_ws:		rec(	%`SBJ_DD_PATH:ACC_ID_REC.DDF`); %^TRmod
Acct_adr_id_ws:		rec(	%`SBJ_DD_PATH:ADR_ID_REC.DDF`); %^TRmod
Temp_adv_inst_ws:	str(4);
Acct_long_ws:		long;
Temp_name1_ws:		vstr(35);
Temp_name2_ws:		vstr(35);
Temp_name3_ws:		vstr(35);
Temp_name4_ws:		vstr(35);
 
Temp_lookup_key_ws:	vstr(64);
Temp_lookup_acc_ws:	vstr(30);
Temp_lookup_adr_ws:	str(5);
Temp_lookup_oneof_ws:	long ;
 
%^ Account lookup stuff.
Save_lookup_id:		str(64);
Lookup_idtype_ws:	str(1);
Lookup_key_ws:		vstr(36);
Lookup_bnk_ws:		str(3);
Search_status_ws:	Boolean;
Acct_que_type_ws:	str(1);		%^"A" = account,
					%^"R" = route,
					%^"D" = destination,
					%^"#" = reference number
					%^"L" = Me_first Index
 
Group_que_type_ws:	str(1);		%^"A" = account,
					%^"R" = route,
					%^"D" = destination,
 
Lsearch_bank_ws:	str(3);         %^ Bank id we actually use for lookup.
Found_id_str_ws:	str(36);        %^ ID we actually found for answerback.
					%^"#" = reference number
 
 
%^ Credit lookup stuff.
Len_ws:			Long;
Idx_ws:			Long;
Match_vstr_ws:		vstr(80);
Temp_vstr_ws:		vstr(80);
Pref_corr_map_site:	oneof(	ibk_pref, bbk_pref, bnp_pref );
Pref_corr_ws:		rec(	%`SBJ_DD_PATH:PARTY_REC.DDF`); %^TRmod
Preferred_cdt_bank_id:	str(3);
 
 
%^ Group lookup stuff.
Lookup_grp_ws:		vstr(24);
 
 
Ansback1_ws:		vstr(24) ;
Ansback2_ws:		vstr(24) ;
Ansback3_ws:		vstr(24) ;
Ansback_match_ws:	vstr(24) ;
Ansback_phase_ws:	word ;
Ansback_spaces_ws:	word ;
 
%^ For special SWIFT lookups
Swift_key1_ws:		vstr(20);
Swift_key2_ws:		vstr(20);
Swift_key_save_ws:      vstr(20);
  
%^ Special ambiguous mode flag.
Rel_part_was_found:             Boolean ;
 

%^****************************************************************************
 
%^ Parse and compose subject definitions.
 
 
Compose_ws:		compose(^NOTRAP);
Parse_ws:		parse(^NOTRAP);
  
Lkup_pid_max_len:         long ;
Lkup_pid_no_suffix:	  Boolean;
Lkup_pid_index:	  	  long ;

%^ PRULE access stuff
Lkp_pr_level:		oneof(%` SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
Lkp_pr_source:		oneof(%` SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
Lkp_pr_ret_stat:	Boolean;
Lkp_pr_type_ws:		vstr(80);
Lkp_pr_ordinal_ws:	long;
Lkp_pr_subtype_ws:	vstr(80);
Lkp_pr_memo:	vstr(%`%ACE$_MSG_STR_SIZE`);
Lkp_pr_stat_ws:	boolean;

Lkp_scan_stat_ws:	boolean;
Lkp_param_name_ws:	vstr(80);
Lkp_param_value_ws:	vstr(80);
Lkp_Param_type_ws:	Oneof(%`SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
Lkp_prms_remaining_ws:	Long;
Lkp_prm_queue_ws:	rec(%`SBJ_DD_PATH:PRIV_ITEM_REC.DDF`);
Lkp_prm_location_ws:	vstr(80);
Lkp_prm_method_ws:	vstr(80);
Rel_bic_8_ok_ws:	str(1);
Address_pending_del:	boolean;
Pending_del_id:		long;
Me_first_bank_count:	word;
Me_first_rel_id:	long;
%end
 


%linkage
01	Lookup_idtype_ls	%Str(1);
01	Lookup_id_ls		%Str(64);
01	Lookup_ovr_byte_ls	%Str(1);
01	Ambig_suppress_ls	%Str(1);
01	Search_all_banks_ls	%Str(1);
01	Search_for_currency_ls	%Str(3);
01	Acct_lkup_pend_del_ls	%Str(1);
01	Acct_lookup_ls		%Boolean ; 

%procedure using
	Lookup_idtype_ls,
	Lookup_id_ls,
	Lookup_ovr_byte_ls,
	Ambig_suppress_ls,
	Search_all_banks_ls,
	Search_for_currency_ls,
	Acct_lkup_pend_del_ls,
 returning	Acct_lookup_ls.
 
 
%^ Lookup the account described by LOOKUP_IDTYPE_LS and LOOKUP_ID_LS.
%^ Return the lookup status in LOOKUP_OVR_BYTE_LS which is blank if the lookup
%^   succeeded unambigiguously, "?" if ambiguous, and "*" if the lookup failed.
%^ If AMBIG_SUPPRESS_LS is "T", then no ambiguity checking is done and the
%^    flag is either " " or "*".
%^ If the idtype is an interbank type (such as "I") then all searches are
%^    implicitly across bank.
%^ In a multi-banking environment, the account ID specified in Lookup_id_ls
%^    may be preceded by a 3 letter bank identifier and a ":".
%^ Relget_return_idtype and Relget_return_key are now set on exit.
%^ If we have substituted an AUX ABA credit party via a SWF to ABA XREF,
%^ Relget_return_idtype and Relget_return_key contain the ABA.
%^ In any case, they always contain the ID that we found.
 

%^****************************************************************************
 
A000_ACCT_LOOKUP.

%^ Assume lookup works.
	Set Success_Is in ACCT_LOOKUP_ls to true. %^  TRmod
        Set Failure_Is in REL_PART_WAS_FOUND to true. %^  TRmod
	%Beg  Relget_msgcode = NULL ;  %End.
	Move spaces to Lookup_ovr_byte_ls.
	Move Lookup_idtype_ls to Lookup_search_idtype_ws.
	Move Lookup_id_ls to Save_lookup_id.
	Move spaces to Relget_return_key, Relget_return_bank.
	Set Failure_Is in DUPLICATE_CHECKED_WF to true. %^  TRmod
	Set Failure_Is in DUPLICATE_FOUND_WF to true. %^  TRmod
	Set Failure_Is in SWIFT_HO_SEARCH_WF to true. %^  TRmod
	Set Failure_Is in AUX_MODE_WF to true. %^  TRmod
	Set Failure_is in Lkup_pid_no_suffix to True.

%^ Check for interbank index idtypes.
	Evaluate Lookup_idtype_ls
	  when "I"
		set Success_Is in INTERBANK_SEARCH_WF to true %^  TRmod
 
	  when other
		set Failure_Is in INTERBANK_SEARCH_WF to true %^  TRmod
	end-evaluate.
 
%^ Determine how 8 character BICs on REL are to be handled
	Call "GCV_REL_BIC_8_OK" using
	    by reference Rel_bic_8_ok_ws.

	Set Failure_is in Address_pending_del to True.
	Move Zero to Pending_del_id.

%^ Parse lookup key (and determine bank id).
	Perform B000_PARSE_LOOKUP_KEY thru B000_PARSE_LOOKUP_KEY_END.
 
%^ Now set bank index.
	%beg
	Bnk_index ^SEARCH (Key = Lookup_bnk_ws);
	%end
	If Failure_Is in BNK_INDEX_STATUS %^  TRmod
	  then	move "*" to Lookup_ovr_byte_ls
		set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
		MOVE Vmsg_lookupfail_wc to Relget_msgcode
		%Beg
		CANCEL: Tran_lkup;
		%End
		%EXIT PROGRAM
	end-if.
 
%^ Kick out blank name lookup here.
	If (Lookup_idtype_ls = "N" OR "@" )
           AND (Lookup_key_ws = SPACES )
	THEN
            move "*" to Lookup_ovr_byte_ls
	    set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
	    MOVE Vmsg_lookupfail_wc to Relget_msgcode
	    %Beg
	    CANCEL: Tran_lkup;
	    %End
	    %EXIT PROGRAM %^ TRmod
	end-if.
 
 
%^ Try searching the specified (or default) bank's indices.
        If (Lookup_idtype_ls = "c" OR "s" OR "$" OR "a" OR "e" )
	    MOVE "*" to Lookup_ovr_byte_ls
	    Set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
%^	    GO TO LOOKUP_JUST_DO_AUX
	    GO TO A000_ACCT_LOOKUP_CLEANUP	%^ Just do AUX lookup.
	END-IF.
	Move Ansback_spaces_ws to Ansback_phase_ws.
	Move SPACE to Acct_que_type_ws.
	Perform B100_ACCT_LOOKUP thru B100_ACCT_LOOKUP_END.
	If (Failure_is in Search_status_ws)
	  AND (Acct_que_type_ws = "L" )
	THEN
	    Perform B100_ACCT_LOOKUP thru B100_ACCT_LOOKUP_END
	END-IF.

%^ Skip multi-bank search if something found or a specific bank was specified
%^   or if only one bank is to be searched or if the search was in an interbank
%^   index.
	If(
	 Failure_Is in acct_lookup_ls
	 and Lookup_ovr_byte_ls is not = "?"
	 and Failure_Is in Bank_specified_wf
	 and Search_all_banks_ls is = "Y"
	 and Lsearch_bank_ws =  Bnk_ID  of Bnk_index  %^ TRmod
	 and Failure_Is in Interbank_search_wf )
	  then	%beg
		Bnk_index ^FIRST;
		%end
 
%^ Already looked in this bank.
		if  Bnk_ID  of Menu_bnk_union is =  Bnk_ID  of Bnk_index
		  then	%beg
			Bnk_index ^NEXT;
			%end
		end-if
 
		perform
		 until(
		  Seq_End_Is  in Bnk_index_cursor 
		  or Success_Is in acct_lookup_ls )
 
%^ Reset flags and variables for this search.
			set Success_Is in ACCT_LOOKUP_LS to true %^  TRmod
			move zero to Relget_msgcode
			move spaces to Lookup_ovr_byte_ls
			move Save_lookup_id to Lookup_id_ls
			Move Ansback_spaces_ws to Ansback_phase_ws
			Move SPACE to Acct_que_type_ws
 
			perform B100_ACCT_LOOKUP thru B100_ACCT_LOOKUP_END
			If (Failure_is in Search_status_ws)
			  AND (Acct_que_type_ws = "L" )
			THEN
			    Perform B100_ACCT_LOOKUP thru B100_ACCT_LOOKUP_END
			END-IF
 
			%beg
			Bnk_index ^NEXT;
			%end
			if  Bnk_ID  of Menu_bnk_union is =  Bnk_ID of Bnk_index
			  then	%beg
				Bnk_index ^NEXT;
				%end
			end-if
		end-perform
	end-if.

%^ LOOKUP_JUST_DO_AUX
A000_ACCT_LOOKUP_CLEANUP.


	If(
	 Failure_Is in acct_lookup_ls
	 and ( Lookup_ovr_byte_ls is = "*"  Or
               ( Lookup_ovr_byte_ls = "?" and	%^ 115326 %^ Still do Cross bank after partials
	      	  Success_is in Rel_part_was_found))) Or
	    ( Acct_lkup_pend_del_ls = "T"  And Lookup_ovr_byte_ls = "?" )	%^ 132811
	  then
		perform B200_AUXILIARY_LOOKUP thru B200_AUXILIARY_LOOKUP_END
		If Acct_lkup_pend_del_ls = "T" and Lookup_ovr_byte_ls = "*"	%^ 132811
		Then	%^ Pending delete on REL, found on AUX
			Move "F" to Acct_lkup_pend_del_ls
		End-if
	end-if



%^	If(
%^	 Failure_Is in acct_lookup_ls
%^	 and Lookup_ovr_byte_ls is = "*" )
%^	  then	perform B200_AUXILIARY_LOOKUP thru B200_AUXILIARY_LOOKUP_END
%^	end-if.
 

* #15526 check for the pending deletion argument and pending deletion date 
	If Success_is in Acct_lookup_ls and
		Success_is in Address_pending_del
	    %ace_conn_root_q Rel_index;
	    %Beg
	    BREAK: Relget_adr_set;
            Rel_index(
            	Key =  Pending_del_id,
                ^SEARCH CONN: Relget_adr_set(nomod) );
            %End
	    move "?" to Lookup_ovr_byte_ls
	    Move "T" to Acct_lkup_pend_del_ls
	    set Failure_is in Acct_lookup_ls to TRUE
	    %Beg  Relget_msgcode = Vmsg_adr_pend_del_wc;  %End
	    %Beg
	    CANCEL: Tran_lkup;
	    %End
	    %EXIT PROGRAM;
	End-if.

	%Beg
	Relget_return_idtype = Lookup_return_idtype_ws ;
	Relget_return_key = Lookup_return_key_ws ;
	Relget_return_bank = Lookup_bnk_ws ;
	%End.


A000_ACCT_LOOKUP_END.

	%Beg
	CANCEL: Tran_lkup;
	%End.

	%EXIT PROGRAM;.
 

%^****************************************************************************
 
B000_PARSE_LOOKUP_KEY.
 
 
%^ This paragraph formats an id for lookup depending on the idtype.
%^ It removes the optional bank id, strips trailing blanks, truncates the key
%^   and find the optional address code if necessary.
 
 
%^ Check for optional bank id and strip trailing blanks.
	%beg
	Parse_ws
	 ^IN(Save_lookup_id);
 
	Parse_ws
	 Lookup_bnk_ws(^STRING<-1>), ":";
	Bank_specified_wf = Parse_ws Status;
 
	Parse_ws
	 Temp_lookup_key_ws, ^SPACE, /;
	%end.
 
%^ Default bank id.
	If Failure_Is in BANK_SPECIFIED_WF %^  TRmod
	  then	move  Bnk_ID  of Menu_bnk_union to Lookup_bnk_ws %^TRmod
	end-if.
 
%^ Truncate large keys.
	If Temp_lookup_key_ws_length is > 36
	  then	move 36 to Temp_lookup_key_ws_length
	end-if.
 
 
%^ Parse account/address code into an account index key record structure.
 
	Move 0 to Ansback_spaces_ws.
 
	Evaluate Lookup_idtype_ls
	  when "D"
	  when "G"
	  when "F"
	  when "V"
	  when "Y"
		Move SPACES to Temp_lookup_acc_ws
		Move SPACES to Temp_lookup_adr_ws
		Move SPACES to Lookup_key_ws
		%beg
		Parse_ws
		 ^IN(Temp_lookup_key_ws),
		 Temp_lookup_oneof_ws(^ONEOF(
		  (|Temp_lookup_acc_ws, "/", |Temp_lookup_adr_ws, "/", /),
		  (|Temp_lookup_acc_ws, "/", /),
		  (|Temp_lookup_acc_ws, "/", |Temp_lookup_adr_ws, /),
		  (|Temp_lookup_acc_ws, /) ) );
		%end
		if Failure_Is in Parse_ws_status
		    Move Temp_lookup_key_ws(1:30) to Lookup_key_ws
		    Move 30 to Lookup_key_ws_length
		else
		    Move Temp_lookup_acc_ws to Lookup_key_ws(1:30)
		    Move Temp_lookup_adr_ws to Lookup_key_ws(31:)
		    Evaluate Temp_lookup_oneof_ws
		        when zero
			    move 35 to Lookup_key_ws_length
		  	when 1
			    move 30 to Lookup_key_ws_length
		  	when 2
			    move 35 to Lookup_key_ws_length
		  	when other
			    move 30 to Lookup_key_ws_length
		    end-evaluate
                end-if
 
 
	  when "X"
%^ Answerbacks are parsed for internal spaces.  They can be in three parts,
%^ separated by spaces, as in aaaaa bbbbb cc.  If we have this format, first
%^ we will try to match the whole thing.  If that fails, we will try to
%^ match the first two parts.  If that fails, we will try to match the
%^ first part.
		%Beg  Lookup_key_ws = Temp_lookup_key_ws ;  %End
		If (Match_3part_xback of Menu_cfg = "T" )
		    %Beg
		    Parse_ws ^IN(Temp_lookup_key_ws), ^NOSPACE_SKIP,
			Ansback1_ws, ^BLANK, Ansback_match_ws, / ;
		    %End
		    If (Success_Is in PARSE_WS_STATUS ) %^  TRmod
		        Add 1 to Ansback_spaces_ws
		        %Beg
		        Parse_ws ^IN(Ansback_match_ws), ^NOSPACE_SKIP,
				Ansback2_ws, ^BLANK, Ansback3_ws, / ;
		        %End
		        If (Success_Is in PARSE_WS_STATUS ) %^  TRmod
		            Add 1 to Ansback_spaces_ws
		        END-IF
		    END-IF
		END-IF
 
	  when "S"
	  when "s"
	  when "$"
		%beg
%^ Eat trailing slashes from address ids.
                Parse_ws ^IN(Temp_lookup_key_ws)
			Lookup_key_ws, "/", / ;
		%End
		If (Failure_Is in Parse_ws_status )
		    %Beg  Lookup_key_ws = Temp_lookup_key_ws;  %End
		END-IF
		Move SPACES to Swift_key1_ws, Swift_key2_ws
		%Beg
		Swift_key1_ws = NULL ;
		Swift_key2_ws = NULL ;
		%End
* Special search setup for SWIFT TID.
* 8-char SWIFT TID becomes 11-character with 3 spaces.  But this is
* equivalent to 3 X's instead of 3 spaces, so we must check both ways.
* For tids with an actual branch code, we just check normally and look
* for dupes if we are supposed to.  For the 8-char tid or its XXX
* branch equivalent, the 3-spaces and 3-X's branch codes are the only
* possible dupes.  (The 8 character tid is NOT read as a partial key for
* a possible 11-character tid.)
*
 		If Lookup_key_ws_length = 8
* 8 character Swift TID maps to BOTH variants.
		    %Beg  Swift_key1_ws = Lookup_key_ws ;  %End
		    Move 11 to Swift_key1_ws_length
		    Set Success_Is in SWIFT_HO_SEARCH_WF to true %^  TRmod
		    %Beg
		    Compose_ws ^OUT(Swift_key2_ws )
			Lookup_key_ws, "XXX", / ;
	    	    %End
		ELSE
		    If (Lookup_key_ws_length = 11 )
	 		AND (Lookup_key_ws(9:3) = SPACES OR "XXX" )
	 	    THEN
	 	        %Beg  Swift_key1_ws = Lookup_key_ws ;  %End
	 	        Set Success_Is in SWIFT_HO_SEARCH_WF to true %^  TRmod
	 		%Beg  Swift_key2_ws = Swift_key1_ws ;  %End
			If (Swift_key1_ws(9:3) = SPACES )
			    Move "XXX" to Swift_key2_ws(9:3)
      			ELSE
			    Move SPACES to Swift_key1_ws(9:3)
			END-IF
		    END-IF
		END-IF
 
	  when other
		%beg
%^ Eat trailing slashes from address ids.
                Parse_ws ^IN(Temp_lookup_key_ws)
			Lookup_key_ws, "/", / ;
		%End
		If (Failure_Is in Parse_ws_status)
		    %Beg  Lookup_key_ws = Temp_lookup_key_ws;  %End
		END-IF
	end-evaluate.
 
 
B000_PARSE_LOOKUP_KEY_END.
 	Exit.

%^****************************************************************************
 
B100_ACCT_LOOKUP.
 
%^ Connect to account, routing, or wire destination index based on specified
%^    idtype.
	%Beg  Lsearch_bank_ws = BNK_INDEX.Bnk_Id ;  %End. %^ TRmod

	If (R_adr_acc_adr_index of Menu_cfg is not = low-values)
	  AND (Acct_que_type_ws NOT = "L" )
	THEN
	    If (Lookup_idtype_ls = "D" or "G" or "F" or "V") %^ Removed "P" from account type
		%ace_conn_root_q Rel_acc_adr_index to Acct_lookup_index always;
		%ace_conn_root_q Rel_index;
		move "L" to Acct_que_type_ws
 		move SPACES to Lsearch_bank_ws
	    END-IF
	ELSE
	    MOVE SPACE to Acct_que_type_ws
	END-IF.
	If (Acct_que_type_ws NOT = "L" )
	    Evaluate Lookup_idtype_ls
	      when "D"
		%ace_conn_root_q Dda_index to Acct_lookup_index always;
		move "A" to Acct_que_type_ws
	      when "G"
		%ace_conn_root_q Gl_index to Acct_lookup_index always;
		move "A" to Acct_que_type_ws
	      when "F"
		%ace_conn_root_q Nostro_index to Acct_lookup_index always;
		move "A" to Acct_que_type_ws

	      when "V"
		%ace_conn_root_q Sav_index to Acct_lookup_index always;
		move "A" to Acct_que_type_ws
	      when "Y"
		%ace_conn_root_q Trade_acc_index to Acct_lookup_index always;
		move "A" to Acct_que_type_ws
 	      when "P"
		%ace_conn_root_q Chips_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws %^ CHIPS is now an R
	      when "N"
		%ace_conn_root_q Sname_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
	      when "A"
		%ace_conn_root_q Aba_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
	      when "S"
		%ace_conn_root_q Swift_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
	      when "B"
		%ace_conn_root_q Branch_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
	      when "C"
		%ace_conn_root_q Chips_uid_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
	      when "U"
		%ace_conn_root_q User_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
%^ spr 27127
	      when "E"
		%ace_conn_root_q Rel_extended_index to Acct_lookup_index always;
		move "E" to Acct_que_type_ws
	      when "I"
		%ace_conn_root_q Rel_interbnk_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
 		move SPACES to Lsearch_bank_ws
	      when "K"
		%ace_conn_root_q Rel_customer_index to Acct_lookup_index always;
		move "R" to Acct_que_type_ws
 		move SPACES to Lsearch_bank_ws
 
	      when "M"
		%ace_conn_root_q Mac_index to Acct_lookup_index always;
		move "D" to Acct_que_type_ws
	      when "T"
		%ace_conn_root_q Dial_index to Acct_lookup_index always;
		move "D" to Acct_que_type_ws
	      when "X"
		%ace_conn_root_q Ans_index to Acct_lookup_index always;
		move "D" to Acct_que_type_ws
	      when "Z"
		%ace_conn_root_q Cable_index to Acct_lookup_index always;
		move "D" to Acct_que_type_ws
 
	      when "R"
		%ace_conn_root_q Rel_index;
		move "#" to Acct_que_type_ws;
 
%^ Invalid idtype.
	      when other
		move "*" to Lookup_ovr_byte_ls
		set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
		MOVE Vmsg_lookupfail_wc to Relget_msgcode
		go to B100_ACCT_LOOKUP_END
	    END-EVALUATE
	END-IF.

	%Beg
	Subject_status_wf = Acct_lookup_index State.Conn; 
	%End.

	If Success_is in Subject_status_wf
	    %Beg
	    Tran_lkup ASSIGN: Acct_lookup_index;
	    %End
	End-if.
 
	If (Success_Is in SWIFT_HO_SEARCH_WF ) %^  TRmod
	    PERFORM X400_SWIFT_SEARCH through X400_SWIFT_SEARCH_END
	ELSE
	    Perform X100_FIND_FIRST_HIT thru X100_FIND_FIRST_HIT_END
        END-IF.
 
	If (Failure_Is in SEARCH_STATUS_WS ) %^  TRmod
	   AND (Ansback_phase_ws NOT = 0 )
	THEN
	    Perform UNTIL (Ansback_phase_ws = 0 )
			  OR (Success_Is in Search_status_ws)
		Evaluate TRUE
		    When (Ansback_phase_ws = 2 )
			%Beg
			Compose_ws ^OUT(Lookup_key_ws), Ansback1_ws,
				" ", Ansback2_ws, / ;
			%End
			Perform X100_FIND_FIRST_HIT through
				X100_FIND_FIRST_HIT_END
			Subtract 1 from Ansback_phase_ws
 
		    When (Ansback_phase_ws = 1 )
		        %Beg  Lookup_key_ws = Ansback1_ws ;  %End
		        Perform X100_FIND_FIRST_HIT through
			        X100_FIND_FIRST_HIT_END
			Subtract 1 from Ansback_phase_ws
		END-EVALUATE
	    END-PERFORM
	END-IF.
 
 
	If Failure_Is in SEARCH_STATUS_WS %^  TRmod
	    %beg  BREAK: Relget_adr_set;  %end
	    If (Acct_que_type_ws NOT = "L" )
                If (Success_is in Rel_part_was_found)
                    Move "?" to Lookup_ovr_byte_ls
                    Move Vmsg_part_lookup_wc to Relget_msgcode
                ELSE
                    Move "*" to Lookup_ovr_byte_ls
                    MOVE Vmsg_lookupfail_wc to Relget_msgcode
                END-IF
                set Failure_Is in ACCT_LOOKUP_LS to true
	    END-IF
	    GO TO B100_ACCT_LOOKUP_END
	END-IF.
 
 
%^ Caller does not want to know if it is ambiguous, just use the first hit.
%^ Also, a Y account must match up to the suffix, so ambiguity cannot occur
	If (Ambig_suppress_ls is = "T" )
	   OR ( (Success_IS in Duplicate_checked_wf)
		AND (Failure_Is in Duplicate_found_wf) )
	   OR (Lookup_idtype_ws = "Y" )
	  then	perform X300_FORMAT_RETURN_KEY thru X300_FORMAT_RETURN_KEY_END
		If Acct_lkup_pend_del_ls = "F" 
		    Evaluate True
			When Pending_del_date of Relget_adr_set > 0 and
	   			Failure_is in Address_pending_del 
		    	    Move Rel_id of Relget_adr_set to Pending_del_id
	    	    	    Set Success_is in Address_pending_del to True
	    	    	    Set Failure_is in Acct_lookup_ls to True
	    		    Move "?" to Lookup_ovr_byte_ls
	    		    Move "T" to Acct_lkup_pend_del_ls
            		    %Beg  Relget_msgcode = Vmsg_adr_pend_del_wc;  %End
			When Pending_del_date of Relget_adr_set = 0 and
				Success_is in Address_pending_del 
		    	    Move Zero to Pending_del_id
	    	    	    Set Failure_is in Address_pending_del to True
		    End-evaluate
		End-if
		go to B100_ACCT_LOOKUP_END
	end-if.
 
 
%^ Found an entry check the next to see whether ambiguous.
	If (Failure_Is in Duplicate_checked_wf)
	THEN
	    Perform X200_CHECK_AMBIGUOUS thru X200_CHECK_AMBIGUOUS_END
        END-IF.
 
%^ Unambiguous match
	If Failure_Is in Duplicate_found_wf
	  then	perform X300_FORMAT_RETURN_KEY thru X300_FORMAT_RETURN_KEY_END
 		If Acct_lkup_pend_del_ls = "F" 
		    Evaluate True
			When Pending_del_date of Relget_adr_set > 0 and
	   			Failure_is in Address_pending_del 
		    	    Move Rel_id of Relget_adr_set to Pending_del_id
	    	    	    Set Success_is in Address_pending_del to True
	    	    	    %^  Set Failure_is in Acct_lookup_ls to True leave success so error will hit.
	    		    Move "?" to Lookup_ovr_byte_ls
	    		    Move "T" to Acct_lkup_pend_del_ls
            		    %Beg  Relget_msgcode = Vmsg_adr_pend_del_wc;  %End
			When Pending_del_date of Relget_adr_set = 0 and
				Success_is in Address_pending_del 
		    	    Move Zero to Pending_del_id
	    	    	    Set Failure_is in Address_pending_del to True
		    End-evaluate
		End-if  
%^ It is ambiguous so disconnect Relget_adr_set.
	  else	%beg
		BREAK: Relget_adr_set;
		%end
		move "?" to Lookup_ovr_byte_ls
		set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
                MOVE Vmsg_ambig_lookup_wc to Relget_msgcode
	end-if.
 
 
B100_ACCT_LOOKUP_END.
	Exit. 

%^****************************************************************************
 
B200_AUXILIARY_LOOKUP.
 
 
%^ When lookup in our REL database fails, this paragraph will try the
%^   auxiliary databases (FED tape, BIC tape, UID tape indexes).
 
%^ Return key will only be returned when the idtype/id change (which only
%^  happens when a SWIFT id is converted to an ABA via the AUX cross-reference
%^  index).

%^  Spr 30746 -add "e" for Aux_ext_id_index lookup
 
%^ BEWARE: the Lookup_return_idtype_ws and Lookup_return_key_ws arguments
%^	   cleared out below can be accessed externally to find out what
%^	   SWIFT ID was found before it was superseded by an ABA cross-lookup.
%^	   Do not change the variable usage here.

	Move spaces to Lookup_return_idtype_ws, Lookup_return_key_ws.
	Move zero to Lookup_return_key_ws_length.
 
%^ Any ID with "/xxxxx" address format can't be trying for auxiliary lookup.
	If Lookup_key_ws_length is > 30
	  then	go to B200_AUXILIARY_LOOKUP_END
	end-if.
 
 
%^ Based on ID-type and lookup party, decide whether to try auxiliary lookup
	Evaluate TRUE
 
%^ SAV and DDA can only apply to the debit and credit parties
	  when (Lookup_idtype_ls = "D" OR "V")
		if(
		 not dbt in Relget_title_flag
		 and not cdt in Relget_title_flag )
		  then	go to B200_AUXILIARY_LOOKUP_END
		end-if
 
%^ ABA's should be looked for unless this is the debit party
	  when (Lookup_idtype_ls = "A" )
		if (dbt in Relget_title_flag )
		   AND (Src_code of Ent_ftr_set is not = "FED" )
		   AND NOT(Src_code of Ent_ftr_set = "SEC"  And
			   tran_type of ent_ftr_set = "ANT")
		THEN
		    go to B200_AUXILIARY_LOOKUP_END
		end-if
 
%^ SWF's should be looked for unless this is the debit
%^ If looking for the credit party, try to convert to an ABA via the AUX
%^   cross-reference index (not useful in source FED case).

	when (Lookup_idtype_ls = "S" )
		Continue

	%^	if dbt in Relget_title_flag 
	%^	  then	go to B200_AUXILIARY_LOOKUP_END
	%^	end-if
	%^	if(
	%^	 cdt in Relget_title_flag
	%^	 and Src_code of Ent_ftr_set is = "FED" )
	%^	  then	go to B200_AUXILIARY_LOOKUP_END
	%^	end-if
 
%^ UID's should be looked for unless this is the debit or credit party
	  when (Lookup_idtype_ls = "C" )
		if(
		 dbt in Relget_title_flag
		 or cdt in Relget_title_flag )
		  then	go to B200_AUXILIARY_LOOKUP_END
		end-if
 
          when (Lookup_idtype_ls = "c" OR "$" OR "s" OR "a" OR "e" or "E")
%^ Caller has specifically asked us to do this.
		Move ZERO to Twiddle_idle_ws	%^ Do-nothing imperative.
 
%^ All other idtypes, just exit
	  when other
		go to B200_AUXILIARY_LOOKUP_END
	end-evaluate.
 
 
%^ We've got a lookup to be attempted, get to appropriate index.
	%ace_is Aux_db_set connected;.
	If Failure_Is in ACE_STATUS_WF %^  TRmod
	  then	%beg
		Dat_root_set.Aux_db_set CONN: Aux_db_set(NOMOD);
		%end
	end-if.
 
	%Beg  BREAK: Acct_lookup_index ;  %End.
 
	Move Lookup_idtype_ls to Lookup_idtype_ws.
	Evaluate Lookup_idtype_ws
	  when "D"
		%beg
		Aux_db_set(
		 .Cif_tape_dda_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "V"
		%beg
		Aux_db_set(
		 .Cif_tape_sav_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "a"
		Move "A" to Lookup_idtype_ws
		%beg
		Aux_db_set(
		 .Fed_tape_aba_index CONN: Acct_lookup_index(read_only) );
		%end

	  when "A"
		%beg
		Aux_db_set(
		 .Fed_tape_aba_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "s"
		Move "S" to Lookup_idtype_ws
		%beg
		Aux_db_set(
		 .Uid_tape_swf_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "c"
		Move "C" to Lookup_idtype_ws
		%beg
		Aux_db_set(
		 .Uid_tape_uid_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "C"
		%beg
		Aux_db_set(
		 .Uid_tape_uid_index CONN: Acct_lookup_index(READ_ONLY) );
		%end
 
	  when "e"
	  when "E"
		%beg
		Aux_db_set(
		 .Aux_ext_id_index CONN: Acct_lookup_index(read_only) );
		%end

	  When "$"
		Move "S" to Lookup_idtype_ws
		%beg
		Aux_db_set(
		  .Bic_tape_swf_index CONN: Acct_lookup_index(READ_ONLY));
		%end
 
	  when "S"

		%^ Not really true any more.... Need to refine this.
		if cdt in Relget_title_flag
%^ A SWIFT idtype in the credit id is only acceptable as a cross-index key to
%^   get an ABA number.
		  then
			Move SPACES to Temp_name_key
			%beg
			Temp_name_key.Idtype = "S" ;
			Compose_ws ^OUT(Temp_name_key.idkey.idacc),
				Lookup_key_ws, / ;
			Aux_db_set.Swf_to_aba_index CONN:
					    Acct_lookup_index(READ_ONLY, EQL) ;
			SEARCH: Acct_lookup_index(Key = Temp_name_key) ;
			Subject_status_wf = Acct_lookup_index Status;
			BREAK: Acct_lookup_index;
			%end
			if Failure_Is in SUBJECT_STATUS_WF %^  TRmod
			    then %beg
			    Aux_db_set.Bic_tape_swf_index CONN:
					Acct_lookup_index(READ_ONLY) ;
			    %end
%^                          GO TO B200_AUXILIARY_SEARCH
			    Perform B210_AUXILIARY_SEARCH thru B210_AUXILIARY_SEARCH_END
			    GO TO B200_AUXILIARY_LOOKUP_END
			end-if
%^ If SWIFT id was found then use the corresponding ABA to search the
%^    SWIFT-to-ABA cross-reference index.
			move Idtype of Disp_id of Acct_lookup_index 
				to Lookup_return_idtype_ws, Lookup_idtype_ws
			move Idkey of Disp_id of Acct_lookup_index(1:9) 
							       to Lookup_key_ws
			move 9 to Lookup_key_ws_length
			Set Failure_Is in SWIFT_HO_SEARCH_WF to true 
%^ When setting up the return ABA Id, don't forget the preferred FED bank (if
%^    any).
			if Preferred_fed_bnk_id of Menu_bnk_union is = spaces
			  then	%beg
				Lookup_return_key_ws = Lookup_key_ws;
				%end
			  else	%beg
				Compose_ws
				 ^OUT(Lookup_return_key_ws)
				 Menu_bnk_union.Preferred_fed_bnk_id, ":",
				 Lookup_key_ws, /;
				%end
			end-if
			%beg
			Aux_db_set(
			 .Fed_tape_aba_index CONN:
						Acct_lookup_index(READ_ONLY) );
			%end
		  else	%beg
			Aux_db_set(
			 .Bic_tape_swf_index CONN:
						Acct_lookup_index(READ_ONLY) );
			%end
 
		end-if
	end-evaluate.

	%Beg
	Subject_status_wf = Acct_lookup_index State.Conn; 
	%End.

	If Success_is in Subject_status_wf
	    %Beg
	    Tran_lkup ASSIGN: Acct_lookup_index;
	    %End
	End-if.
 
%^ B200_AUXILIARY_SEARCH.
	Perform B210_AUXILIARY_SEARCH thru B210_AUXILIARY_SEARCH_END.

B200_AUXILIARY_LOOKUP_END.
	Exit.

%^****************************************************************************

B210_AUXILIARY_SEARCH.

        Move SPACES to Lsearch_bank_ws.
	Set Success_Is in AUX_MODE_WF to true. %^  TRmod
	Set Failure_Is in Search_status_ws to true.
	If (Success_Is in SWIFT_HO_SEARCH_WF ) %^  TRmod
	    PERFORM X400_SWIFT_SEARCH through X400_SWIFT_SEARCH_END
%^	    GO TO B200_AUXILIARY_RESULTS
	    PERFORM B215_AUXILIARY_RESULTS thru B215_AUXILIARY_RESULTS_END
	    GO TO B210_AUXILIARY_SEARCH_END
	END-IF.
%^ Try a search, exit on failure
	%beg
	Acct_lookup_index ^SEARCH (EQL, .Rel_name_key(
	  					     .Idbank = null,
	  					     .Idtype = Lookup_idtype_ws,
	  					     .Idkey = Lookup_key_ws ) );
	%end.
 
 
	If (Success_Is in ACCT_LOOKUP_INDEX_STATUS ) %^  TRmod
	    Set Success_Is in Search_status_ws to true
%^ Found something, get name and address data
	    If (Lookup_idtype_ws = "D" OR "V" or "E")
		%Beg
	        BREAK: Relget_adr_set;
		Acct_lookup_index CONN: Temp_null_seq(NOMOD,
			TOP: Temp_rel_union(NOMOD,
				.Adr_set CONN: Relget_adr_set(NOMOD) ) ) ;
	        BREAK: Temp_rel_union;
	        BREAK: Temp_null_seq;
                %End
	    ELSE	       
	        %Beg
	        BREAK: Relget_adr_set;
	        Acct_lookup_index CONN: Relget_adr_set(NOMOD);
		%End
	    END-IF


	    %^ AIN check no longer required. Since AUX must be connected for
	    %^ Rules processing, this will happen back in Lookups

	    %beg
           	Relget_adr_set (etrap);
	    	%^ MAINTAIN CONNECTION 	    BREAK: Relget_adr_set;
	    %end
	END-IF.
 
	If (Success_Is in SEARCH_STATUS_WS ) %^  TRmod
	   AND ( (Failure_Is in Duplicate_checked_wf )
	       AND (Ambig_suppress_ls NOT = "T" ) )
	THEN
	    Perform X200_CHECK_AMBIGUOUS thru X200_CHECK_AMBIGUOUS_END
        END-IF.
 
	Perform B215_AUXILIARY_RESULTS thru B215_AUXILIARY_RESULTS_END.

B210_AUXILIARY_SEARCH_END.
	Exit. 

%^****************************************************************************

%^B200_AUXILIARY_RESULTS.
B215_AUXILIARY_RESULTS.

	If (Success_Is in SEARCH_STATUS_WS ) %^  TRmod
	    If (Success_Is in DUPLICATE_FOUND_WF ) %^  TRmod
%^ Clean up subject connections.
		move "?" to Lookup_ovr_byte_ls
		set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
                MOVE Vmsg_ambig_lookup_wc to Relget_msgcode
%^127281	%beg
%^127281	BREAK: Acct_lookup_index;
%^127281	%end
	    ELSE
%^ Callers will recognize this form of LOOKUP "failure" if they care.
		move Vmsg_dat_notonfile_wc to Relget_msgcode
		set Failure_Is in ACCT_LOOKUP_LS to true %^  TRmod
                Move Idtype of Rel_name_key of Acct_lookup_index %^TRmod
			to Lookup_return_idtype_ws
%^ Use subject so length will be set - important for vstrs
%^		Move Idkey of Rel_name_key of Acct_lookup_index %^TRmod
%^			to Lookup_return_key_ws
		%Beg Lookup_return_key_ws = Acct_lookup_index.Rel_name_key.Idkey; %End
		Move SPACES to Lookup_bnk_ws
 
%^ Clean up subject connections.
	    END-IF
	ELSE
 
	    %beg
	    BREAK: Acct_lookup_index;
	    %end
	end-if.

B215_AUXILIARY_RESULTS_END.
	Exit.

%^****************************************************************************
 
X100_FIND_FIRST_HIT.
 
 
%^ This paragraph looks for the first hit in a partial search.
%^ "Partial" searches are inefficient, so a greater-than-or-equal search is
%^     done using the appropriate index.
 
 
	Set Failure_Is in Search_status_ws to true.
	Evaluate Acct_que_type_ws
	  when "A"
	  when "E"
		%beg
		BREAK: Relget_adr_set;
		Acct_lookup_index(
		 GEQ, FORWARD,
		 .Rel_name_key(
		  .Idbank = Lsearch_bank_ws,
		  .Idtype = Lookup_search_idtype_ws,
		  .Idkey = Lookup_key_ws ),
		 ^SEARCH CONN: Temp_null_seq(
		  nomod,
		  TOP: Temp_rel_union(
		   nomod,
		   .Adr_set CONN: Relget_adr_set(nomod) ) ) );
 
		Search_status_ws = Acct_lookup_index Status;
 
		BREAK: Temp_rel_union;
		BREAK: Temp_null_seq;
		%end
 
 
	  when "R"
		%beg
		BREAK: Relget_adr_set;
		Acct_lookup_index(
		 GEQ, FORWARD,
		 .Rel_name_key(
		  .Idbank = Lsearch_bank_ws,
		  .Idtype = Lookup_search_idtype_ws,
		  .Idkey = Lookup_key_ws ),
		 ^SEARCH CONN: Relget_adr_set(
		  nomod,
		  TOP: Temp_rel_union(nomod) ) );
 
		Search_status_ws = Acct_lookup_index Status;
 
		BREAK: Temp_rel_union;
		%end
 
 
	  when "L"
		%beg
		BREAK: Relget_adr_set;
		SEARCH: Acct_lookup_index(
		 GEQ, FORWARD,
		 .Rel_name_key(
		  .Idbank = Lsearch_bank_ws,
		  .Idtype = Lookup_search_idtype_ws,
		  .Idkey = Lookup_key_ws ));

		Search_status_ws = Acct_lookup_index Status;
		%end
		If Success_is in Search_status_ws
			%beg
                	Rel_index(
                   	Key = Acct_lookup_index.Rel_id,
                   	^SEARCH CONN: Relget_adr_set(nomod) );
                	%end
		end-if

	  when "D"
		%beg
		BREAK: Relget_adr_set;
		Acct_lookup_index(
		 GEQ, FORWARD,
		 .Rel_name_key(
 		  .Idbank = Lsearch_bank_ws,
		  .Idtype = Lookup_search_idtype_ws,
		  .Idkey = Lookup_key_ws ),
		 ^SEARCH CONN: Temp_null_set(
		  nomod,
		  TOP: Temp_rel_union(
		   nomod,
		   .Adr_set CONN: Relget_adr_set(nomod) ) ) );
 
		Search_status_ws = Acct_lookup_index Status;
 
		BREAK: Temp_rel_union;
		BREAK: Temp_null_set;
		%end
 
 
	  when "#"
		%beg
		BREAK: Acct_lookup_index;
		BREAK: Relget_adr_set;
		Parse_ws
		 ^IN(Lookup_key_ws),
		 REL_INDEX.Rel_key, /;
		Rel_index(notrap);
		Relget_adr_set(notrap);
		Rel_index( EQL,
		 ^SEARCH CONN: Relget_adr_set(nomod) );
		Rel_index(etrap);
		Relget_adr_set(etrap);
		%end
 
		if(
		 Success_Is in Parse_ws_status
		 and address_is in Rel_type of Rel_index) %^TRmod
		  then	set Success_Is in Search_status_ws to true
 
		  else	set Failure_Is in Search_status_ws to true
			%beg
			BREAK: Relget_adr_set;
			%end
		end-if
		go to X100_FIND_FIRST_HIT_END
	end-evaluate.
 
 
	If Failure_Is in SEARCH_STATUS_WS %^  TRmod
	  then	go to X100_FIND_FIRST_HIT_END
	end-if.
 
 
%^ Search for SWIFT ID and Fedwire ABA is considered to fail unless
%^   the search key is long enough and fully matches the key in the index.
%^   A SWIFT ID must match in the 1st 11 characters even if it is 8 characters:
%^   for an 8 character SWIFT ID, a match is exact if there are 3 trailing
%^   spaces in the index key.
%^   spr# 3586, move on-file answerback ID length to Acct_long_ws to require
%^   a full match on answerbacks.
%^ Consideration:
%^    SHOULD OTHER LOOKUPS BE EXACT MATCHES? i.e. "U", "I", MAC-CODE
	Evaluate Lookup_idtype_ls
	  when "A"
		move 9 to Acct_long_ws
	  when "C"
		move 6 to Acct_long_ws
	  when "P"
		If (Lookup_key_ws(1:Lookup_key_ws_length) is NUMERIC )
%^ Allow address match on numeric part of PID key
		    Move Lookup_key_ws_length to Acct_long_ws
		    Move Idkey of Rel_name_key of Acct_lookup_index to
								Found_id_str_ws
		    %beg
		    Parse_ws ^IN(Found_id_str_ws)
				Temp_vstr_ws, ^SPACE, / ;
		    %end
		    Add 1 to Lookup_key_ws_length giving Lkup_pid_index
		    If (Temp_vstr_ws_length > Acct_long_ws )
		       AND (Found_id_str_ws(Lkup_pid_index:1) is ALPHABETIC)
		    THEN
			Set Success_is in Lkup_pid_no_suffix to TRUE
		    END-IF
		ELSE
%^ Looks like it has a suffix, so be strict.
                    Move zeroes to Lkup_pid_max_len
%^ Get "P" idtype maximum length from the Bnk_union clearinghouse sequence
	            Call "GET_PID_MAX_LENGTH" using
		        by Reference Lsearch_bank_ws
                        by reference Lookup_key_ws
                        by reference Lookup_key_ws_length
		        by reference Lkup_pid_max_len
		    If (Lkup_pid_max_len = 0 )
%^ No such thing as a Pid here.
	                set Failure_Is in Search_status_ws to TRUE
		        GO TO X100_FIND_FIRST_HIT_END
		    END-IF
		    Move Lkup_pid_max_len to Acct_long_ws
		END-IF
	  when "S"
		move 11 to Acct_long_ws
	  when "X"
		Move SPACES to Found_id_str_ws
		Move Idkey of Rel_name_key of Acct_lookup_index to
			Found_id_str_ws
		%beg
		Parse_ws ^IN(Found_id_str_ws)
			Temp_vstr_ws, ^SPACE, / ;
		%end
		move Temp_vstr_ws_length to Acct_long_ws
	  when "Y"
* Must be either whole key or whole key up to suffix.
		%Beg  Temp_vstr_ws = Acct_lookup_index.Rel_name_key.Idkey;  %End
		Move Temp_vstr_ws_length to Acct_long_ws
		If (Lookup_key_ws_length NOT = Temp_vstr_ws_length )
		    Add 1 to Lookup_key_ws_length giving acct_long_ws
		    If (Lookup_key_ws(1:Lookup_key_ws_length) is numeric)
			AND (Idkey of Rel_name_key of
			      Acct_lookup_index(Acct_long_ws:1) is NOT numeric)
		    THEN
			MOVE Lookup_key_ws_length to Acct_long_ws
		    END-IF
		END-IF	

	  when other
		move Lookup_key_ws_length to Acct_long_ws
	end-evaluate.
 
	If(
	 Idbank of Rel_name_key of Acct_lookup_index is not = 
							Lsearch_bank_ws
	 OR Idtype of Rel_name_key of Acct_lookup_index is not =  
							Lookup_search_idtype_ws
	 OR Idkey of Rel_name_key of Acct_lookup_index(1:Acct_long_ws)
			       is not = Lookup_key_ws(1:Lookup_key_ws_length) )
          THEN
%^ We missed.  See about flagging a partial match.
            set Failure_Is in Search_status_ws to TRUE
	    GO TO X100_FIND_FIRST_HIT_END
	END-IF.

        If (Partial_match_ambig of Menu_cfg = "T" )
	   AND (Failure_is in Lkup_pid_no_suffix )
	THEN
%^ Check for a partial match.
            If (Lookup_key_ws_length < Acct_long_ws )
               AND (Idkey of Rel_name_key of
			Acct_lookup_index(1:Lookup_key_ws_length)
                          = Lookup_key_ws(1:Lookup_key_ws_length) )
            THEN
                Set Success_Is in REL_PART_WAS_FOUND to true  %^TRmod
            END-IF
            go to X100_FIND_FIRST_HIT_END
	end-if.
 
X100_FIND_FIRST_HIT_END.
 	Exit.

%^****************************************************************************
 
X200_CHECK_AMBIGUOUS.
 
 
%^ This paragraph checks current index item to see if it matches the
%^    Relget_adr_set data.  For purposes of SWIFT ID's, an 8 character ID
%^    is not considered ambiguous relative to an 11 character ID with the same
%^    first 8 characters.
%^ Output: search_status_ws = success if ID's match, else failure
%^ TODO - use search currency if specified to resolve ambiguities.
 
 
%^ Reference number index has no duplicate entries.
	If Acct_que_type_ws is = "#"
	  then	set Failure_Is in DUPLICATE_FOUND_WF to true %^  TRmod
		go to X200_CHECK_AMBIGUOUS_END
	end-if
 
 
%^ Found an entry check the next to see whether ambiguous.
	%beg
	Acct_lookup_index ^NEXT;
	Acct_adr_id_ws = ACCT_LOOKUP_INDEX.Rel_name_key; 
	Acct_lookup_index ^PREV;
	%end.
 
 
%^ If the SWIFT ID length is 8, make it 11 so that 3 trailing spaces are
%^    significant in the comparison
	If(
	 Lookup_idtype_ls is = "S"
	 and Lookup_key_ws_length is = 8 )
	  then	move 11 to Acct_long_ws
	  else	move Lookup_key_ws_length to Acct_long_ws
	end-if.
 
 
%^ Check for another hit which makes the first hit ambiguous.
	If (Success_is in Lkup_pid_no_suffix)
	   OR (Idtype of Acct_adr_id_ws is not = Lookup_search_idtype_ws)
	   OR (Idkey of Acct_adr_id_ws(1:Acct_long_ws) is not =
						Lookup_key_ws(1:Acct_long_ws) )

%^ #116715 Remove check for a specific length if the idtype is "B" (branch).
%^	   The lengths are determined in B000_PARSE_LOOKUP_KEY.

	   OR (Lookup_idtype_ls = "B" And
	  	 Idkey of Acct_adr_id_ws is not = Lookup_key_ws)

	  then	set Failure_Is in DUPLICATE_FOUND_WF to true 
 
%^ A second match means the key was ambiguous.
	  else	set Success_Is in DUPLICATE_FOUND_WF to true 
		If Acct_que_type_ws = "L"
		    Perform X210_ME_FIRST_CHECK_AMBIG thru X210_ME_FIRST_CHECK_AMBIG_END
		End-if
	end-if.
 
 
X200_CHECK_AMBIGUOUS_END.
 	Exit.

X210_ME_FIRST_CHECK_AMBIG.

	Move Zero to Me_first_bank_count, Me_first_rel_id.

	%Beg
	BREAK: Temp_lookup_index;
	Acct_lookup_index EQUATE: Temp_lookup_index;
	%End.

	Perform until (Seq_end_is in Temp_lookup_index_cursor) or
		(Idtype of Rel_name_key of Temp_lookup_index not = Lookup_search_idtype_ws) or
	   	(Idkey of Rel_name_key of Temp_lookup_index(1:Acct_long_ws) not = Lookup_key_ws(1:Acct_long_ws))
	    If Idbank of Disp_id of Temp_lookup_index = Lookup_bnk_ws
		Add 1 to Me_first_bank_count
		Move Rel_id of Temp_lookup_index to Me_first_rel_id
	    End-if
	    %Beg
	    NEXT:Temp_lookup_index;
	    %End
	End-perform.

	%Beg
	BREAK: Temp_lookup_index;
	%End.

	If Me_first_bank_count = 1
	    Set Failure_is in Duplicate_found_wf to True
	    %Beg
	    SEARCH: Rel_index(forward, eql, Key=Me_first_rel_id);
	    BREAK: Relget_adr_set;
	    Rel_index CONN: Relget_adr_set(nomod);
	    %End
	End-if.

X210_ME_FIRST_CHECK_AMBIG_END.
	EXIT.

%^****************************************************************************
 
X300_FORMAT_RETURN_KEY.
 
 
	Move Lookup_idtype_ls to Lookup_return_idtype_ws.
 
	Evaluate Lookup_idtype_ls
	  when "D"
	  when "V"
	  when "G"
	  when "F"
	  when "Y"
		%beg
		Compose_ws
		 ^OUT(Lookup_return_key_ws),
		ACCT_LOOKUP_INDEX.Rel_name_key.Idkey(.Idacc,"/",.Idadr),/;
		%end
	  when other
		move space to Lookup_return_idtype_ws
		move spaces to Lookup_return_key_ws
		move zero to Lookup_return_key_ws_length
	end-evaluate.
 
	If Lookup_return_key_ws_length is not = zero
	  then	move Lookup_return_key_ws(1:Lookup_return_key_ws_length)
								to Lookup_id_ls
	end-if.
 
 
X300_FORMAT_RETURN_KEY_END.
	Exit. 

%^***************************************************************************
 
X400_SWIFT_SEARCH.
* Paragraph to actually do special swift search.  We are looking for both
* the SPACES branch code and the "XXX" branch code avatars of an 8-character
* SWIFT TID.  Order is significant; Swift_key1_ws is the one which we are
* told to look for and Swift_key2_ws is its equivalent.  An ambiguity check
* is mandatory.
 
        Set Failure_Is in DUPLICATE_FOUND_WF to true.

        Move spaces to Swift_key_save_ws.
 
	%beg
	BREAK: Relget_adr_set;
	Acct_lookup_index(EQL, FORWARD, .Rel_name_key(
		  			  .Idbank = Lsearch_bank_ws,
		  			  .Idtype = "S",
		  			  .Idkey = Swift_key1_ws ),
		 ^SEARCH CONN: Relget_adr_set(NOMOD) ) ;
 
	Search_status_ws = Acct_lookup_index Status;
	%End.
 
        If (Success_Is in SEARCH_STATUS_WS) then %^  TRmod
            %^ Save the successful search key for later
            %Beg Swift_key_save_ws = Swift_key1_ws; %End
        ELSE
	    Move 8 to Swift_key1_ws_length
	    %Beg
	    Acct_lookup_index(EQL, Forward, .Rel_name_key(
		  			.Idbank = Lsearch_bank_ws,
		  			.Idtype = "S",
		  			.Idkey = Swift_key1_ws ),
		 ^SEARCH CONN: Relget_adr_set(NOMOD) ) ;
	    Search_status_ws = Acct_lookup_index Status;
	    %End
            If (Success_is in Search_status_ws) then
                %^ Save the successful search key for later
                %Beg Swift_key_save_ws = Swift_key1_ws; %End
	    END-IF
	END-IF.
 
	If (Failure_Is in Search_status_ws)
	    %beg
	    Acct_lookup_index(EQL, FORWARD, .Rel_name_key(
		  			.Idbank = Lsearch_bank_ws,
		  			.Idtype = "S",
		  			.Idkey = Swift_key2_ws ),
		 ^SEARCH CONN: Relget_adr_set(NOMOD) ) ;
 
	    Search_status_ws = Acct_lookup_index Status;
	    %End
            If (Success_Is in SEARCH_STATUS_WS) then
                %^ Save the successful search key for later
                %Beg Swift_key_save_ws = Swift_key2_ws; %End
            end-if
            IF (Success_Is in SEARCH_STATUS_WS) %^TRmod
               AND (Ambig_suppress_ls is not = "T" )
               AND (Failure_Is in Duplicate_found_wf)
                    Perform X500_SWF_AMBIG_CHECK thru
                            X500_SWF_AMBIG_CHECK_end
            END-IF
  	ELSE
            If (Ambig_suppress_ls is not = "T" )
               AND (Failure_Is in Duplicate_found_wf)
                    Perform X500_SWF_AMBIG_CHECK thru
                            X500_SWF_AMBIG_CHECK_end
            END-IF
* We are just cross-checking for a duplicate.
	    %beg
	    SEARCH: ACCT_LOOKUP_INDEX.Rel_name_key(
		  			.Idbank = Lsearch_bank_ws,
		  			.Idtype = "S",
		  			.Idkey = Swift_key2_ws ) ;
	    %End
            If Success_Is in ACCT_LOOKUP_INDEX_STATUS  %^TRmod
                Set Success_Is in DUPLICATE_FOUND_WF to true  %^TRmod
            END-IF
	END-IF.
 
	If (Success_Is in SEARCH_STATUS_WS ) %^  TRmod
	    SET Success_Is in DUPLICATE_CHECKED_WF to true %^  TRmod
            If NOT(Success_Is in AUX_MODE_WF ) %^  TRmod
		%^ Removed AIN checking, since AUXwill be left connected, this
		%^ this will be picked up in the RULES processing
		%^             ELSE
		%Beg
	    	Relget_adr_set TOP: Temp_rel_union(NOMOD) ;
	    	BREAK: Temp_rel_union;
	    	%End
	    END-IF
	ELSE
            If (Partial_match_ambig of Menu_cfg = "T" ) and
	       (Rel_bic_8_ok_ws not = "T")
%^ Check for a partial match.
                Move 8 to Swift_key1_ws_length
                %Beg
                BREAK: Relget_adr_set;
                SEARCH: Acct_lookup_index(GEQ,
                                        FORWARD,
                                        .Rel_name_key( 
                                                .Idbank = Lsearch_bank_ws,
                                                .Idtype = "S",
                                                .Idkey = Swift_key1_ws ) ) ;
                Search_status_ws = Acct_lookup_index Status;
                Acct_lookup_index(EQL) ;
                %End
                If (Success_Is in SEARCH_STATUS_WS ) 
                    If (Swift_key1_ws(1:Swift_key1_ws_length ) =
                        Idkey of Rel_name_key of
                        Acct_lookup_index(1:Swift_key1_ws_length) )
                    THEN
%^ A partial match.
                        Set Success_Is in REL_PART_WAS_FOUND to true  %^TRmod
                    END-IF
                END-IF
            END-IF
  	    %Beg  BREAK: Acct_lookup_index ;  %End
            Set Failure_Is in Search_status_ws to TRUE
	END-IF.
 
        If Swift_key_save_ws not = spaces
          then
            %^ Reposition index at the successful search key
            %beg
            SEARCH: Acct_lookup_index(
                EQL, FORWARD,
                .Rel_name_key(
                    .Idbank = Lsearch_bank_ws,
                    .Idtype = "S",
                    .Idkey = Swift_key_save_ws ) ) ;
            %End
        end-if.
 
X400_SWIFT_SEARCH_END.
 	Exit.

X500_SWF_AMBIG_CHECK.
 
%^ Found an entry check the next to see whether ambiguous.
        %beg
        Acct_lookup_index ^NEXT;
        Acct_adr_id_ws = ACCT_LOOKUP_INDEX.Rel_name_key; %^ TRmod
        Acct_lookup_index ^PREV;
        %end.
 
%^ Check for another hit which makes the first hit ambiguous.
        If( Acct_adr_id_ws is = Rel_name_key of Acct_lookup_index )
%^ A second match means the key was ambiguous.
          then  set Success_Is in DUPLICATE_FOUND_WF to true 
        end-if.
 
X500_SWF_AMBIG_CHECK_END.
 	Exit.

%^****************************************************************************
 
%module	GRP_LOOKUP
 
 
%linkage
01 Lookup_type_ls		%Str(1);
01 Lookup_id_ls			%Str(24);
01 Lookup_status_ls		%Str(1);
01 Ambig_suppress_ls		%Str(1);
01 Grp_lookup_ls           	%Boolean;

%procedure using
	Lookup_type_ls,
	Lookup_id_ls,
	Lookup_status_ls,
	Ambig_suppress_ls
  Returning
	Grp_lookup_ls.
 
 
%^ Look up the group specified in Lookup_id_ls.
%^ If Lookup_type_ls is "1", the Grp_id_index is searched; if "2", the
%^   Grp_name_index is searched.
%^
%^ Return the lookup status in Lookup_status_ls which is:
%^	blank if the lookup succeeded,
%^	"*" if it failed,
%^	"?" if the id is ambiguous.
%^ If Ambig_suppress_ls is "T", then no ambiguity checking is done and the
%^  flag is either " " or "*".
%^
%^ Group Ids are unique across banks.
%^ Duplicate group names are allowed.
 
 

%^****************************************************************************
 
A000_GRP_LOOKUP.
 
 
	Set Failure_Is in Grp_lookup_ls to True.
	Move SPACES to Lookup_status_ls
	Move Lookup_type_ls to Lookup_search_idtype_ws.
	Move Lookup_id_ls to Lookup_grp_ws(1:%siz(Lookup_grp_ws)).
	Move %siz(Lookup_grp_ws) to Lookup_grp_ws_length.
 
	%beg
	Parse_ws
	 ^IN(Lookup_grp_ws),
	 Lookup_grp_ws, ^SPACE, /;
	%end.
 
%^ for name lookup
	If Lookup_type_ls is = "2"
	  then	%ace_conn_root_q Rel_grp_name_index to Grp_lookup_index always;
 
 
%^ for id lookup
	  else	if Lookup_grp_ws_length is > 10
		  then	move "T" to Lookup_status_ls
			Set Success_Is in Grp_lookup_ls to TRUE
			%EXIT PROGRAM %^ TRmod
		end-if
 
		%ace_conn_root_q Rel_grp_id_index to Grp_lookup_index always;
	end-if.
 
	Perform B100_INDEX_SEARCH thru B100_INDEX_SEARCH_END.
 
A000_GRP_LOOKUP_END.

	 %EXIT PROGRAM.
 

%^****************************************************************************
 
B100_INDEX_SEARCH.
 
 
%^ Search the Group ID or Name index for the specified group.  Also check to
%^   see if the specified ID or name is ambiguous.
 
 
	%beg
	BREAK: Ent_grp_set;
	Grp_lookup_index ^SEARCH (
	 GEQ, FORWARD,
	 .Rel_name_key(
	  .Idbank = null,
	  .Idtype = Lookup_search_idtype_ws,
	  .Idkey = Lookup_grp_ws ) );
	%end.
 
%^ No match.
	If(
	 Failure_Is in Grp_lookup_index_status
	 or Idtype of Rel_name_key of Grp_lookup_index is not = 
							Lookup_search_idtype_ws
	 or Idkey of Rel_name_key of
		Grp_lookup_index(1:Lookup_grp_ws_length) is not =
					Lookup_grp_ws(1:Lookup_grp_ws_length) )
	  then	move "*" to Lookup_status_ls
		go to B100_INDEX_SEARCH_END
	end-if.
 
%^ Exact match.
	If(
	 Idkey of Rel_name_key of Grp_lookup_index is = Lookup_grp_ws
	 and(
	  Lookup_type_ls is = "1"
	  or(
	   Lookup_type_ls is = "2"
	   and Ambig_suppress_ls is = "T" ) ) )
	  then	%beg
		BREAK: Relget_adr_set;
		Grp_lookup_index CONN: Relget_adr_set(NOMOD);
		%end
		Set Success_Is in Grp_lookup_ls to true
		go to B100_INDEX_SEARCH_END
	end-if.
 
%^ Partial match not allowed.
	If Ambig_suppress_ls is = "T"
	  then	move "*" to Lookup_status_ls
		go to B100_INDEX_SEARCH_END
	end-if.
 
%^ Check to see if the id/name specified is ambiguous.
	%beg
	Grp_lookup_index ^NEXT;
	%end.
 
	If(
	 Success_Is in Grp_lookup_index_status
	 and Idtype of Rel_name_key of Grp_lookup_index is = 
						      Lookup_search_idtype_ws
	 and Idkey of Rel_name_key of
			Grp_lookup_index(1:Lookup_grp_ws_length) is = 
					Lookup_grp_ws(1:Lookup_grp_ws_length) )
	  then	%beg
%^ position at first match
		Grp_lookup_index ^PREV;
		%end
%^ plan to send selection screen
		move "?" to Lookup_status_ls
 
%^ direct hit
	  else	%beg
		BREAK: Relget_adr_set;
		Grp_lookup_index ^PREV CONN: Relget_adr_set(NOMOD);
		%end
		Set Success_Is in Grp_lookup_ls to true
	end-if.
 
 
B100_INDEX_SEARCH_END.
 
 

%^****************************************************************************
 
 
%module	NEXT_ACCT_LOOKUP
 
 
%linkage
01	Lookup_idtype_ls	%Str(1);
01	Lookup_id_ls		%Str(64);
01	Lookup_ovr_byte_ls	%Str(1);
01 Next_acct_ls           	%Boolean;

%procedure using
	Lookup_idtype_ls,
	Lookup_id_ls,
	Lookup_ovr_byte_ls,
 returning	Next_acct_ls.
 
%^ ACCT_LOOKUP already Looked up the ADDRESS described by LOOKUP_IDTYPE_LS and
%^ LOOKUP_ID_LS.  It found ambiguous results.  The caller wants to step
%^ through the addresses to resolve the ambiguity.  We will return the next
%^ address unCONNected in the Relget_adr_set if there is one and we will
%^ return status SUCCESS.  If there are no more we will not change the
%^ Relget_adr_set and we will return status FAILURE.
%^ The Acct_long_ws, Acct_adr_id_ws,  Acct_lookup_index, and Acct_que_type_ws
%^ subjects set up by ACCT_LOOKUP are used as implicit arguments to this
%^ routine.

%^****************************************************************************
 
A000_NEXT_LOOKUP.
 
%^ Assume lookup fails
	Set Failure_Is in Next_acct_ls to true. %^  TRmod
	Move Vmsg_lookupfail_wc to Relget_msgcode.
	If Acct_que_type_ws = "#"
*  Rel id cannot be ambiguous
	    GO TO A000_NEXT_LOOKUP_END
	END-IF.
	%Beg  NEXT: Acct_lookup_index ;	  %End
	If (Success_Is in ACCT_LOOKUP_INDEX_STATUS ) %^  TRmod
	   AND (Idtype of Acct_adr_id_ws =
			    Idtype of Rel_name_key of Acct_lookup_index) %^TRmod
	   AND (Idkey of Acct_adr_id_ws(1:Acct_long_ws) =
	     Idkey of Rel_name_key of Acct_lookup_index(1:Acct_long_ws) ) %^TRmod
	THEN
*  Found suitable "next" ambiguous address.
	    %Beg  BREAK: Relget_adr_set ;  %End
	    Evaluate Acct_que_type_ws
		when "A"
		when "E"
		    %Beg
		    Acct_lookup_index CONN: Temp_null_seq(NOMOD,
		  	TOP: Temp_rel_union(NOMOD,
		   	    .Adr_set CONN: Relget_adr_set(NOMOD) ) );
		    BREAK: Temp_rel_union;
		    BREAK: Temp_null_seq;
		    %End
 
		when "R"
		    %Beg
		    Acct_lookup_index CONN: Relget_adr_set(NOMOD) ;
		    %End
 
		when "D"
		    %Beg
		    Acct_lookup_index CONN: Temp_null_set(NOMOD,
		  	TOP: Temp_rel_union(NOMOD,
		   		.Adr_set CONN: Relget_adr_set(NOMOD) ) ) ;
		    BREAK: Temp_rel_union;
		    BREAK: Temp_null_set;
		    %End

	  	when "L"
			%beg
                	Rel_index(
                   		Key = Acct_lookup_index.Rel_id,
                   		^SEARCH CONN: Relget_adr_set(NOMOD) );
                	%end

	    END-EVALUATE
	    %Beg  BREAK: Relget_adr_set ;  %End
	    Set Success_Is in Next_acct_ls to true %^  TRmod
	    %Beg  Relget_msgcode = NULL ;  %End
	END-IF.

A000_NEXT_LOOKUP_END.

	%EXIT Program.
	

%^****************************************************************************
 
%^	ADS.3 A. Smith	6-DEC-1983
%^	Optimize to avoid outputting data if the override flag is already
%^	correct.
%^
%^	D. Beer		31-DEC-1987
%^	Copy logic from CREDIT_EDIT to avoid creating an advise type of FED
%^	when the source code is FED.
%^
%^	A. Smith	8-OCT-1988
%^	Change ambiguity check for SWIFT ID's - if searching for an
%^	8 character SWIFT ID, it is not ambiguous with an 11 character
%^	ID with the same leading 8 characters.
%^
%^	T. Schultz	18-Oct-88
%^	ACCT_LOOKUP now looks for a bank ID in the lookup Id passed in.
%^	eg. check for things like "FNB:00000000".
%^
%^	D. Beer		19-Oct-1988
%^	In DEBIT_LOOKUP, move key returned by ACCT_LOOKUP (if any) into DBT_ID.
%^
%^	T. Schultz	21-Oct-1988
%^	Add GRP_LOOKUP routine.
%^
%^	D.Beer		28-Dec-1988
%^	Never change Ent_debit_set id type to ABA or CHIPS if not set that
%^	way by mapper.
%^
%^	D.Beer		17-Jan-89
%^	Save Ent_debit_set commission and cable charge flags from account.
%^
%^	D.Beer		23-Jan-89
%^	Call common subroutine CREDIT_ADVICE_LOOKUP (in ENTPAY) to find an
%^	appropriate advice type.
%^
%^	A. Smith	12-Feb-1989
%^	Remove code from CDT_LOOKUP to new subroutine COPY_CDT_DATA to allow
%^	separate call when address and account have been connected.
%^
%^	D. Beer		18-Feb-1989
%^	Create new subroutine COPY_DBT_DATA to separate call when
%^	Relget_adr_set and account have been connected.
%^	Fill DBT_CURRENCY in DEBIT_LOOKUP when in multi-currency environment.
%^	Add Menu_cfg fsect for FX_ENA flag.
%^
%^	D. Beer		3-Mar-1989
%^	Clear ACCT_LOOKUP's return VSTR length. Fixes bug where DBT_ID's were
%^	getting erased after the first 1031 in FEDIN.
%^
%^	T. Schultz	27-Mar-1989
%^	When looking up a branch, fill in the DBT/Cdt_account field.
%^
%^	D. Beer		21-Apr-1989
%^	When applicable, copy Ent_adv_set data after calling
%^	CREDIT_ADVICE_LOOKUP.
%^
%^	D. Beer		10-Jun-1989
%^	On REL lookup failure, try the auxiliary database of FED, SWF, UID
%^	or CIF names/addresses.
%^
%^	D. Beer		28-Aug-1989
%^	Resurrect preferred correspondent logic. And refine it to avoid
%^	over-writing existing BBK data.
%^
%^	D. Beer		1-Sep-1989
%^	Fix preferred corresp logic to copy name and address data and set
%^	appropriate advising instructions.
%^
%^	D. Beer		17-Jan-1990
%^	Always compare at least 14 characters of ID on account idtypes.
%^	This handles unwanted partial compare at banks with multiple account
%^	number lengths (like UBD).
%^
%^	D.Beer		25-Apr-1990
%^	Enhance the "preferred correspondent" logic to deal with INTRTL
%^	global preferred correspondents.  Also, make the logic take advantage
%^	of IBK data when BBK and BNF are full. And, copy ENTBNP's logic for
%^	setting BNP_BNK_FLG when preferred correspondent causes us to use BNP
%^	block.
%^
%^	F.Hanna 28-AUG-1990 15:28:21.22
%^	Fill in debit/credit prod codes.
%^
%^	Fred .M. Hanna	19-NOV-1990 12:17:03.22
%^	Fill in payment SWIFT id for advices which specify one.
%^
%^	A. Smith	27-Nov-1990
%^	Disallow matches of Fed ABA, CHIPS UID, CHIPS participant and
%^	8-character SWIFT identifiers where a short key happens to
%^	uniquely match an entry in the index being searched.
%^
%^	A. Smith	5-Jan-1991
%^	Return from CREDIT_LOOKUP and DEBIT_LOOKUP with RELGET_ADR_SET
%^	connected if the override-flag is blank, which means there
%^	was an unambiguous match. The caller may need the connection.
%^
%^	D. Beer		29-Aug-1991
%^	Connect ENT_C_ADR_SET before calling CREDIT_ADVICE_LOOKUP.
