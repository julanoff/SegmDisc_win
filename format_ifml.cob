%MODULE FORMAT_IFML;
***************************************************************************
*                                                                         *
* © 2007-2008 ACI Worldwide, Inc. and its affiliates. All rights reserved.*
* This media contains confidential and proprietary information of ACI     * 
* Worldwide, Inc. or one of its affiliates. No disclosure of use of this  *
* material, its contents or any prior use thereof may be made without the *
* prior express written consent of ACI Worldwide, Inc.                    *
*                                                                         *
***************************************************************************
******************************************************************
*                                                                *
* Copyright (c) 1999 - 2007 by IntraNet, Inc.                    *
* All rights reserved.                                           *
*                                                                *
* This Software is confidential and proprietary to IntraNet and  *
* it is protected by U.S. copyright law, other national          *
* copyright laws, and international treaties.  The Software may  *
* not be disclosed or reproduced in whole or in part in any      *
* manner to any third party without the express prior written    *
* consent of IntraNet, Inc.                                      *
*                                                                *
* This Software and its related Documentation are made available *
* under the terms of the IntraNet Software License and may not   *
* be used, reproduced or disclosed in any manner except as       *
* expressly authorized by the Software License.  All other uses  *
* are strictly prohibited.                                       *
*                                                                *
* This Software and its related Documentation are proprietary    *
* and confidential material of IntraNet, Inc.                    *
*                                                                *
******************************************************************

*******************************************************************************
*
* Create ifml/xml version of the current entfrt_fsect message trn.
*
* Calling sequence:
*
*	CALL "FORMAT_IFML" USING
*	    BY REFERENCE options_ws             %^ Input
*	    BY CONTENT "<app id>"               %^ Input "ISI2-XML" or
*						%^       "STP-ACE "
*	RETURNING rtn_stat_wf.
*
* Caller is responsible for populating entftr_fsect including ent_text_seq
* and ent_dst_set (a disconnected ent_dst_set means no dst set).
*
* Format_ifml keys off of the ent_dst_set connection.
*
* When there is a connection to ent_dst_set, the caller is telling us to
* limit our ent_msg_history, ent_text_set, and ent_dst_set XML formatting
* to this destination.
*
* When there is not a connection to ent_dst_set, the caller is telling us to
* XML format all the destination data and their associated text sequences and
* message history.
*
* When there is not a connection to ent_dst_set, the caller is reponsible for
* setting ent_msg_history and ent_text_seq.
*
* When there is a connection to ent_dst_set, the caller is still responsible
* for setting ent_text_seq (which will be used as the 'default text seq' for
* destinations that do not have a uniqueue text seq. 
*
* options_ws - which prule levels we want to consider when formatting prules
*      POS LEVEL  VALUE = Y or N
*      --- -----
* 	1  msg_is 
* 	2  sbk_is 
* 	3  dbt_party_is 
* 	4  cdt_party_is 
* 	5  dest_is 
* 	6  sbk_prof_is 
* 	7  dbt_prof_is 
* 	8  cdt_prof_is 
* 	9  sbk_bank_is 
*	10 dbt_bank_is 
*	11 cdt_bank_is 
*	12 system_is 
*	13 snd_cnf_is 
*	14 dbt_cnf_is 
*	15 cdt_cnf_is 
*	16 all_cnf_is
*
* 
* Upon unexpected xml formatting error, FORMAT_IFML will issue a broadcast
* message listing the (1st) problem point - xml tag and data and will return
* failure_is.
*
* Design Notes:
*
* The resultant XML is required to never have 'parent' tags present without
* subordinate 'child' data.
* So 2 complete passes of the message data are made.
* Pass 1 sets numerous Boolean fields that corrrespond to 'parent' tag data.
* Then Pass 2 only composes a 'parent' tag when its corresponding Boolean field
* is set.
*
* Different iMTS callers require subsets of the total IFML (need some
* formatting supressed or customized).
* This is accomplished by using another set of Booleans - 1 per caller type.
* The caller ids themself via the app_id_ls linkage parameter.
* A boolean is set and used as an xml formatting constraint. 
* Pass 1 (see 'Pass 1' description above) uses these Booleans to suppress
* identifying tags as present and/or compose-able.
* If the item being suppressed is an elementary data item (non-parent tag),
* there also must exist code in Pass 2 (see 'Pass 2' description above) code to
* suppress the composing - for composing at this level is not controlled by
* Pass 1 Booleans.
* WHEN MAINTAINING THE 'OMIT FIELD/GROUP LOGIC BASED ON app_id_ls, HANDLE THE
* PASS 2 COMPOSING ASPECT WHEN NOT 'GROUP FLAG' CONTROLLED (IE. WHEN THE
* FIELD LENGTH (NON 0) OTHERWISE DETERMINES ITS APPEARENCE OR NOT IN THE XML). 
*
* Prule formatting is really custom per caller so explicit tests on the caller
* control logic flow here (example 	IF app_id_ls = "STP-ACE" THEN).
* Other formatting is thought to be skipped on an exception basis so logic
* flow is based on skipping specific caller types (example
* IF success_is IN omit_4_isi2_wf). 

*******************************************************************************

* Modification History

* R. Gadzinski		07-Jun-2005	123085
*	Divorce formatting from stp_ace.cob for sharing with isi2xml_xmtedt.
*	But here conform to the 'scope test' and just copy the code.
*	Due to isi2 requirements, we now compose out to ent_text_seq instead
*	of to a large buffer.
*	Add the app_id_ls linkage parameter and associated logic to suppress
*	xml field formatting by caller.
*
* R. Gadzinski		20-Oct-2005	123085
*	React to 125051/126144.  Apparently we cannot count on ent_text_seq
*	actually being its defined type of dat_text_seq.  It might be of type
*	dat_wide_text_seq.
*	Also when a dest set is passed by the caller (probably only possible
*	for STP-ACE calls - STP-ACE does not use us in 1.2), the ent_text_seq
*	data failed to compose. 
*
* R. Gadzinski		27-Oct-2005	123085
*	Stop formatting TertiaryID data for isi2.
*	Correct the <CreditPartyInf><AccountPartyInf><AdrBankID> tag hierarchy.
*	Format <PaymentDates> for isi2-xml (and stp).
*
* C. Boddy		28-Nov-2005	127461
*	If the ENT_FTR_SET.AMOUNT is 0, then set the source to ADM. THis to address
*	the inability of PRIME (the OFAC Scanner) to identify admins - apparently
*	they seem unable to get it right except when we set the source to ADM. SInce the 
*	source in this case is not needed or used, it seems a reasonable approach in order to 
*	hit the 12/19 live date.
*
* R. Gadzinski		28-Nov-2005	127475
*	Allow first line blank in text sequences - this is normal for FED.
*
* R. Gadzinski		09-Jan-2006	128077 128175
*	Stop SBJALRCON trap on fi_text_seq.
*
* R. Gadzinski		07-Mar-2006	129178
*	Restore original oneof values for the prule <Level> field.
*
* R. Gadzinski		12-Sep-2007	icr_003216 140414
*	Provide custom selection of <SourceCode> formatting - see 127461.
*
* B. Heath    		26-Nov-2007	icr_003308 141360
*	Bring up to standard for java translation.
*
* R. Gadzinski		26-Oct-2007	140417 CR1008
*	Default <Currency> to bank currency when <Amount> present and
*	currency blank - isi2xml only.
*
* End Modification History
*******************************************************************************

%DEF  	<ACE> %`SBJ_DD_PATH:ACE_FSECT.DDL` %END
%def	<ENTFTR> %`SBJ_DD_PATH:entftr_fsect.ddl` %end

%def
all_execs_option_wf:		Boolean; 
AT_wf:				Boolean;
AT_DH_wf:			Boolean; 
AT_MH_wf:			Boolean; 
AT_MT_wf:			Boolean;
BBI_wf:				Boolean;
been_there_done_that_wf:	Boolean; 
BP_AI_wf:			Boolean; 
BP_wf:				Boolean;
BBI_BPI_wf:			Boolean;
BPI_BPI_wf:			Boolean;
BPI_wf:				Boolean; 
BR_wf:				Boolean; 
Cfg_error_msg_ws:		vstr(80);
Cfg_item_data_ws:   		vstr(256);
Cfg_item_key_ws:		vstr(25);
Cfg_item_type_ws:		vstr(16);
Cfg_seq_ordinal_ws:		word;
Cfg_sts_ws:			boolean;
Cfg_union_key_ws:       	rec (%`SBJ_DD_PATH:CFG_ID_REC.DDF`);
compose_lz_ws:			Compose(^NOTRAP, ^LEADING_ZEROS);
Compact_compose_ws:		compose(^NOTRAP);
compose_ok:			Boolean;
compose_seq_ws:			seq(%`SBJ_DD_PATH:DAT_TEXT_2048_SEQ.DDF`);
compose_ws:			Compose(^Notrap); 
counter_ws:			Long;
CPI_API_AIDI_wf:		Boolean;
CPI_API_CA_wf:			Boolean;
CPI_API_SID_AIDI_wf:		Boolean; 
CPI_API_SID_wf:			Boolean;
CPI_API_TID_AIDI_wf:		Boolean; 
CPI_API_TID_wf:			Boolean; 
CPI_API_wf:			Boolean; 
CPI_wf:				Boolean;
currency_code_ws:		Vstr(3); 
data_of_1st_failure_ws:		Vstr(100);
datetime_ws:			Time; 
DPI_API_AIDI_wf:		Boolean;
DPI_API_CA_wf:			Boolean; 
DPI_API_SID_AIDI_wf:		Boolean; 
DPI_API_SID_wf:			Boolean;
DPI_API_TID_AIDI_wf:		Boolean; 
DPI_API_TID_wf:			Boolean; 
DPI_API_wf:			Boolean;
fi_dst_set:			set(%`SBJ_DD_PATH:DST_SET.DDF`);

%^ Sequence of ent_dst_set data
fi_fi_dst_seq:			seq(%`SBJ_DD_PATH:DAT_TEXT_SEQ.DDF`);

%^ Sequence of text seqs
fi_fi_text_seq:			seq(%`SBJ_DD_PATH:DAT_TEXT_SEQ.DDF`);

fi_msg_history:			seq(%`SBJ_DD_PATH:MSG_HISTORY_SEQ.DDF`);
fi_text_seq:			seq(%`SBJ_DD_PATH:DAT_TEXT_SEQ.DDF`);
first_time_called_wf:		Boolean;
first_prule_wf:			Boolean;
formatting_isi2_wf:		Boolean; 
get_next_tbl_item_wf:		Boolean; 
got_one_wf:			Boolean; 
grp_1_ws:			Word; 
grp_2_ws:			Word; 
grp_3_ws:			Word; 
grp_4_ws:			Word; 
grp_5_ws:			Word; 
grp_6_ws:			Word; 
ifml_compose:			Compose(^Notrap);
ifml_ws:			Vstr(32000);
IB1_BPI_wf:			Boolean;
IB1_wf:				Boolean; 
IBI_BPI_wf:			Boolean;
IBK_BPI_wf:			Boolean;
IBK_wf:				Boolean; 
level_ws:			Vstr(80);
lcl_bnk_union:			Set(%`SBJ_DD_PATH:BNK_UNION.DDF`);
loop_done_wf:			Boolean; 
M_wf:				Boolean;
max_level_ws:			Word; 
new_executable_wf:		Boolean; 
new_rule_wf:			Boolean; 
next_state_wf:			Oneof(
				    done_is,
				    get_config_is,
				    get_next_prule_is,
				    get_next_exec_is,
				    get_next_all_exec_is,
				    got_one_is,
				    got_one_all_is );
OBI_BPI_wf:			Boolean;
omit_4_isi2_wf:			Boolean;
omit_4_stp_wf:			Boolean; 
OPI_BPI_wf:			Boolean;
option_ws:			Word; 
Parse_ws:			Parse (^Notrap, ^Space_skip); 
PI_wf:				Boolean; 
pr_get_side_wf:			Oneof(%`SBJ_DD_PATH:PRULE_SIDE_ONEOF.DDF`);
Pr_level_ws:			Oneof(%`SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
Pr_level_wrk_ws:		Oneof(%`SBJ_DD_PATH:PRULE_LEVEL_ONEOF.DDF`);
Pr_memo_ws:			Vstr(%`%ACE$_MSG_STR_SIZE`);
pr_msglevel_ws:			Oneof(%`SBJ_DD_PATH:PRULE_MSGLEVEL_ONEOF.DDF`);
Pr_ordinal_ws:			Long;
Pr_source_ws:			Oneof(%`SBJ_DD_PATH:PRULE_SOURCE_ONEOF.DDF`);
Pr_subtype_ws:			Str(80);
Pr_was_there: 			Boolean;
PR_wf:				Boolean;
prev_prule_param_ws:		Vstr(40);
prparm_count_ws: 		long;
prparm_edit_wf:			oneof(%` SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
prparm_value_ws:		Vstr(80);
prule_found_wf:			Boolean;
prule_param_prev_ws:		Vstr(40); 
Prule_param_remain_ws:		long;
Prule_param_type_ws:		oneof(%`SBJ_DD_PATH:PR_PARAM_EDIT_ONEOF.DDF`);
Prule_param_value_ws:		vstr(80);
Prule_param_ws:			vstr(40);
prule_selectable_wf:		Boolean; 
RLI_wf:				Boolean; 
Rs_wf:				Boolean; 
rs2_wf:				Boolean; 
SBI_BPI_wf:			Boolean; 
Sts_ws:				Boolean;
tag_id_ws:			Vstr(80);
tag_idtype_ws:			Vstr(80);
tag_of_1st_failure_ws:		Vstr(80);
tag_ws:				Vstr(80);
tbl_caller_ws:			Vstr(8);
tbl_id_ws: 			Vstr(80);
tbl_prule_ws:          		Vstr(40);
Temp_subtagvar_ws: 		Vstr(60);
Temp_tagvar_ws: 		Vstr(400);
Temp_tagvar1_ws: 		Vstr(240);
text_ardy_in_seq_wf:		Boolean;
tmp_bool:			Boolean; 
vstr80_ws:			Vstr(80);
Vstr132_ws:			Vstr(132);
str3_ws:			str(3);
we_exit_wf:			Boolean;
which_level_ws:			Long; 
which_seq_ws:			Word;
wrk_main_history:		seq(%`SBJ_DD_PATH:MSG_HISTORY_SEQ.DDF`);
wrk_msg_history:		seq(%`SBJ_DD_PATH:MSG_HISTORY_SEQ.DDF`);
wrk_msg_union:			set(%`SBJ_DD_PATH:MSG_UNION.DDF`);
xml3p_adm_wf:			Boolean; 
%end

%Linkage

01 options_ls		%Str(80);	%^ Input
01 app_id_ls		%Str(8);	%^ Input
01 return_stat_ls	%Boolean;	%^ Output

%PROCEDURE USING
    BY REFERENCE options_ls
    BY REFERENCE app_id_ls
RETURNING
    return_stat_ls.

a000_main.

	IF failure_is IN been_there_done_that_wf THEN
	    PERFORM b000_one_time_only THRU b000_one_time_only_end
	    %beg been_there_done_that_wf success_is; %end
	END-IF.

	%beg
	compose_ok success_is;
	tag_of_1st_failure_ws = null; 
	%end.

	PERFORM c300_compose_xml THRU c300_compose_xml_end.

* A failure was more likely to happen back when we were composing into a buffer
* rather than into the seq that we now compose into.
	IF failure_is IN compose_ok THEN
	    SET failure_is IN return_stat_ls TO true
	    IF app_id_ls = "ISI2-XML" THEN
		CALL "NEX_CREATE_AND_BROADCAST_MSG" USING
		    BY CONTENT Z"XML$_BAD_COMPOSE",
		    BY VALUE -1,
		    %ace_msg_arg_list(ent_ftr_set.trn_ref.trn_date,
			ent_ftr_set.trn_ref.trn_num,
			tag_of_1st_failure_ws,
			data_of_1st_failure_ws);
	    ELSE
		CALL "NEX_CREATE_AND_BROADCAST_MSG" USING
		    BY CONTENT Z"STP$_BAD_COMPOSE",
		    BY VALUE -1,
		    %ace_msg_arg_list(ent_ftr_set.trn_ref.trn_date,
			ent_ftr_set.trn_ref.trn_num,
			tag_of_1st_failure_ws,
			data_of_1st_failure_ws);
	    END-IF
	ELSE
	    SET success_is IN return_stat_ls TO true
	END-IF.

a000_main_end.
	%EXIT PROGRAM.

b000_one_time_only.
*
* 1 time only initialization on first call
*
* Set the omit_<number>_wf boolean that suppress XML per our app_id_ls
*
	IF app_id_ls = "ISI2-XML" THEN
	    %beg
	    formatting_isi2_wf success_is; 
	    omit_4_isi2_wf success_is;
	    %end
	END-IF.

* For 3.0
	IF app_id_ls = "STP-ACE " THEN
	    %beg omit_4_stp_wf success_is; %end
	END-IF.

	CALL "CUST_XML3P_ADM" RETURNING xml3p_adm_wf.

b000_one_time_only_end.
	EXIT.

c300_compose_xml.
*
* Everything is optional.  In theory there could be nothing to format.
* We require that there be no parent tags without a child.
* Sooooooo we need to first determine which parent tag levels we are
* actually going to later format.
* This will make more sense if you have the IFML structure displayed.
* Basically, the Cobol paragraph levels correspond to the IFML structure
* levels.

* Crawl through the IFML structure setting booleans to indicate which parent
* tags we need.
	PERFORM d100_determine_parent_tags THRU d100_determine_parent_tags_end.

* And now we re-use ent_text_seq as our output XML sequence
	%beg
	Break: ent_text_seq;
	ent_text_seq(Mod); 
	Alloc_temp: ent_text_seq;
	%end.

* Crawl through the IFML structure again actually doing the IFML composing.
	PERFORM d200_use_parent_tags THRU d200_use_parent_tags_end. 

c300_compose_xml_end.
	EXIT.

d100_determine_parent_tags.
*
* Message  M_wf
*   BasicPayment  BP_wf
*   AuditTrail  AT_wf  

	PERFORM e100_BasicPayment THRU e100_BasicPayment_end.
	PERFORM e200_AuditTrail THRU e200_AuditTrail_end.

	IF failure_is IN BP_wf
	 AND failure_is IN AT_wf THEN
* nothing at this level
	    %beg M_wf failure_is; %end
	ELSE
* we have data to compose at this level
	    %beg M_wf success_is; %end
	END-IF.

d100_determine_parent_tags_end.
	EXIT.

d200_use_parent_tags.
*
* Compose the IFML (if any)
*
* Message  M_wf
*   BasicPayment  BP_wf
*   AuditTrail  AT_wf

	%beg ifml_compose ^Out(ent_text_seq.txt),^Line_wrap,^Noword_wrap,^Trailing_blanks; %end.

	IF success_is IN M_wf THEN
* Do all the upper parent tags that preceed the actual first data
	    %beg ifml_compose '<?xml version="1.0" encoding="UTF-8" ?>'; %end
	    %XML_compose "IFML";
	    %XML_compose "File";
	    %XML_compose "Message";

	    IF success_is IN BP_wf THEN
		PERFORM e300_BasicPayment THRU e300_BasicPayment_end
	    END-IF

	    IF success_is IN AT_wf THEN
		PERFORM e400_AuditTrail THRU e400_AuditTrail_end
	    END-IF

	    %XML_compose "/Message";
	    %XML_compose "/File";
	    %XML_compose "/IFML";
	END-IF. 

* Finish off the last seq item (^line_wrap already handled the rest)
	%beg
	ifml_compose /, ^Alloc_elem;
%^ Avoid uncommitted 'max tran' issues
	Commit_temp: ent_text_seq;
	%end.

d200_use_parent_tags_end.
	EXIT.

e100_BasicPayment.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     RepetitiveLineInf  RLI_wf
*     BatchRef  BR_wf
*     AccountInf  BP_AI_wf
*     PartyInf  PI_wf
*     ProcessingRule  PR_wf

	%beg First: ent_related_amount_seq; %end.

	PERFORM f000_RepetitiveLineInf THRU f000_RepetitiveLineInf_end.
	PERFORM f100_BatchRef THRU f100_BatchRef_end.
	PERFORM f200_AccountInf THRU f200_AccountInf_end.
	PERFORM f300_PartyInf THRU f300_PartyInf_end.
	PERFORM f400_ProcessingRule THRU f400_ProcessingRule_end

* Handle the xml formatting exclusion requirements of the caller
	IF success_is IN omit_4_isi2_wf
	OR ( dbt_draft_num OF ent_debit_set = SPACE ) THEN
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 exch_rate OF ent_ftr_set = 0
	 AND dbt_exch_rate OF ent_debit_set = 0
	 AND cdt_exch_rate OF ent_credit_set = 0) THEN
	    %beg grp_2_ws = <0>; %end
	ELSE
	    %beg grp_2_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 regulatory_report1 OF ent_credit_set = SPACE
	 AND regulatory_report2 OF ent_credit_set = SPACE
	 AND regulatory_report3 OF ent_credit_set = SPACE) THEN
	    %beg grp_3_ws = <0>; %end
	ELSE
	    %beg grp_3_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 funds_type OF typ OF ent_ftr_set = SPACE) THEN
	    %beg grp_4_ws = <0>; %end
	ELSE
	    %beg grp_4_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 instr_adv_type OF ent_ftr_set = SPACE
	 AND split_ctr OF ent_ftr_set = 0
	 AND rpr_level OF ent_ftr_set = SPACE) THEN
	    %beg grp_5_ws = <0>; %end
	ELSE
	    %beg grp_5_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 caller OF cal OF ent_ftr_set = SPACE
	 AND trader_ctrl OF ent_ftr_set = SPACE
	 AND riskiness_ind OF ent_ftr_set = SPACE
	 AND dbt_chrg OF chrg OF ent_ftr_set = SPACE
	 AND cdt_chrg OF chrg OF ent_ftr_set = SPACE
	 AND commission OF chrg OF ent_ftr_set = SPACE
	 AND cbl_charge OF chrg OF ent_ftr_set = SPACE
	 AND comm_mode OF chrg OF ent_ftr_set = SPACE
	 AND cbl_mode OF chrg OF ent_ftr_set = SPACE
	 AND orig_exch_rate OF ent_ftr_set = 0
	 AND bank_operation_code OF ent_ftr_set = SPACE
	 AND bopr_text1 OF ent_ftr_set = SPACE
	 AND bopr_text2 OF ent_ftr_set = SPACE
	 AND bopr_text3 OF ent_ftr_set = SPACE
	 AND NUL_is IN msg_state OF ent_ftr_set
	 AND NUL_is IN high_msg_state OF ent_ftr_set
	 AND none IN dbt_chrg_org OF ent_ftr_set
	 AND none IN cdt_chrg_org OF ent_ftr_set
	 AND none IN comm_chrg_org OF ent_ftr_set
	 AND none IN cbl_chrg_org OF ent_ftr_set) THEN
	    %beg grp_6_ws = <0>; %end
	ELSE
	    %beg grp_6_ws = <1>; %end
	END-IF.

	IF trn_ref OF ent_ftr_set = SPACE
	 AND orig_inst_val_date OF ent_ftr_set = 0
	 AND inst_date OF ent_ftr_set = 0
	 AND inst_time OF ent_ftr_set = 0
	 AND send_date OF ent_ftr_set = 0
	 AND hh OF release_time OF ent_ftr_set = SPACE
	 AND mm OF release_time OF ent_ftr_set = SPACE
	 AND date_time OF due_date OF ent_ftr_set = 0
	 AND none IN change_state OF due_date OF ent_ftr_set
	 AND date_time OF prime_send_date OF ent_ftr_set = 0
	 AND none IN change_state OF prime_send_date OF ent_ftr_set
	 AND date_time OF second_send_date OF ent_ftr_set = 0
	 AND none IN change_state OF second_send_date OF ent_ftr_set
	 AND date_time OF dbt_value_date OF ent_debit_set = 0
	 AND none IN change_state OF dbt_value_date OF ent_debit_set
	 AND date_time OF dbt_book_date OF ent_debit_set = 0
	 AND none IN change_state OF dbt_book_date OF ent_debit_set
	 AND date_time OF cdt_value_date OF ent_credit_set = 0
	 AND none IN change_state OF cdt_value_date OF ent_credit_set
	 AND date_time OF cdt_book_date OF ent_credit_set = 0
	 AND none IN change_state OF cdt_book_date OF ent_credit_set
	 AND grp_1_ws = 0
	 AND bank OF loc_info OF ent_ftr_set = SPACE
	 AND loc OF loc_info OF ent_ftr_set = SPACE
	 AND grp_2_ws = 0
	 AND currency_code OF ent_ftr_set = SPACE
	 AND dbt_currency OF ent_debit_set = SPACE
	 AND cdt_currency OF ent_credit_set = SPACE
	 AND sec_cur OF dbt_acc_2 OF ent_debit_set = SPACE
	 AND sec_cur OF cdt_acc_2 OF ent_credit_set = SPACE
	 AND ter_cur OF dbt_acc_3 OF ent_debit_set = SPACE
	 AND ter_cur OF cdt_acc_3 OF ent_credit_set = SPACE
* check status of above First
	 AND failure_is IN ent_related_amount_seq_status 
	 AND amount OF ent_ftr_set = 0
	 AND base_amount OF ent_ftr_set = 0
	 AND dbt_amount OF ent_debit_set = 0
	 AND cdt_amount OF ent_credit_set = 0
	 AND sec_amt OF dbt_acc_2 OF ent_debit_set = 0
	 AND sec_amt OF cdt_acc_2 OF ent_credit_set = 0
	 AND ter_amt OF dbt_acc_3 OF ent_debit_set = 0
	 AND ter_amt OF cdt_acc_3 OF ent_credit_set = 0
	 AND failure_is IN RLI_wf
* ignore reg_idtype without a reg_id
	 AND reg_id OF ent_ftr_set = SPACE
	 AND grp_3_ws = 0
	 AND tran_type OF typ OF ent_ftr_set = SPACE
	 AND src_code OF ent_ftr_set = SPACE
	 AND type_code OF ent_ftr_set = SPACE
	 AND subtype OF ent_ftr_set = SPACE
	 AND grp_4_ws = 0
	 AND incoming_msgtype OF ent_ftr_set = SPACE
	 AND incoming_format OF ent_ftr_set = SPACE
	 AND incoming_ref OF ent_ftr_set = SPACE
	 AND failure_is IN BR_wf
	 AND grp_5_ws = 0
	 AND failure_is IN BP_AI_wf
	 AND grp_6_ws = 0
	 AND dbt_bnk_inf1 OF ent_debit_set = SPACE
	 AND dbt_bnk_inf2 OF ent_debit_set = SPACE
	 AND dbt_bnk_inf3 OF ent_debit_set = SPACE
	 AND dbt_bnk_inf4 OF ent_debit_set = SPACE
	 AND dbt_bnk_inf5 OF ent_debit_set = SPACE
	 AND dbt_bnk_inf6 OF ent_debit_set = SPACE
	 AND orp_ben_inf1 OF ent_credit_set = SPACE
	 AND orp_ben_inf2 OF ent_credit_set = SPACE
	 AND orp_ben_inf3 OF ent_credit_set = SPACE
	 AND orp_ben_inf4 OF ent_credit_set = SPACE
	 AND failure_is IN PI_wf
	 AND failure_is IN PR_wf THEN
	    %beg BP_wf failure_is; %end
	ELSE
	    %beg BP_wf success_is; %end
	END-IF.

e100_BasicPayment_end.
	EXIT.

e200_AuditTrail.
*
* Message  M_wf
*   AuditTrail  AT_wf
*     DstHistory  AT_DH_wf
*     MsgText     AT_MT_wf
*     MsgHistory  AT_MH_wf

	PERFORM f150_DstHistory THRU f150_DstHistory_end.
	PERFORM f250_MsgText THRU f250_MsgText_end.
	PERFORM f450_MsgHistory THRU f450_MsgHistory_end.

	IF failure_is IN AT_DH_wf
	 AND failure_is IN AT_MT_wf
	 AND failure_is IN at_MH_wf THEN
* nothing at this level
	    %beg AT_wf failure_is; %end
	ELSE
	    %BEG AT_WF success_is; %end
	END-IF.

e200_AuditTrail_end.
	EXIT.

e300_BasicPayment.
*
* Compose the BasicPayment data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     RepetitiveLineInf  RLI_wf
*     BatchRef  BR_wf
*     AccountInf  BP_AI_wf
*     PartyInf  PI_wf
*     ProcessingRule  PR_wf
	%XML_compose "BasicPayment";

	%XML_compose "TransactionUID" ent_ftr_set.trn_ref "/TransactionUID";

	IF orig_inst_val_date OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "OrigInstVal"];
	    %XML_compose "DateTime"
		ent_ftr_set.orig_inst_val_date.slashed_yyyymmdd "/DateTime";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF inst_date OF ent_ftr_set = 0
	 AND inst_time OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    IF inst_time OF ent_ftr_set NOT = 0 THEN
		%XML_compose "PaymentDates" ["Type" "Inst"];
		%beg
		compose_lz_ws ^Out( vstr80_ws),
		    ent_ftr_set.inst_time.hh(^Num<2>), ":",
		    ent_ftr_set.inst_time.mn(^Num<2>), ":",
		    ent_ftr_set.inst_time.ss(^Num<2>), /;
		%end
		%XML_compose "DateTime" vstr80_ws "/DateTime";
		%XML_compose "/PaymentDates";
	    ELSE
		%XML_compose "PaymentDates" ["Type" "Inst"];
		%XML_compose "DateTime" ent_ftr_set.inst_date.slashed_yyyymmdd
		    "/DateTime";
		%XML_compose "/PaymentDates";
	    END-IF
	END-IF.

	IF send_date OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "Send"];
	    %XML_compose "DateTime" ent_ftr_set.send_date.slashed_yyyymmdd
		"/DateTime";
	    %XML_compose"/PaymentDates";
	END-IF.

	IF hh OF release_time OF ent_ftr_set = SPACE
	 AND mm OF release_time OF ent_ftr_set = SPACE THEN
	    CONTINUE
	ELSE
	    %beg
	    compose_lz_ws ^Out(vstr132_ws)
		ent_ftr_set.release_time.hh, ":"
		ent_ftr_set.release_time.mm, ":",
		"00" /;
	    %end
	    %XML_compose "PaymentDates" ["Type" "Release"];
	    %XML_compose "DateTime" vstr132_ws "/DateTime";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF due_date OF ent_ftr_set = 0
	 AND none IN change_state OF due_date OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "Due"];
	    %beg datetime_ws = ent_ftr_set.due_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_ftr_set.due_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF prime_send_date OF ent_ftr_set = 0
	 AND none IN change_state OF prime_send_date OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "PrimeSend"];
	    %beg datetime_ws = ent_ftr_set.prime_send_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_ftr_set.prime_send_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF second_send_date OF ent_ftr_set = 0
	 AND none IN change_state OF second_send_date OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "SecondSend"];
	    %beg datetime_ws = ent_ftr_set.second_send_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_ftr_set.second_send_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF dbt_value_date OF ent_debit_set = 0
	 AND none IN change_state OF dbt_value_date OF ent_debit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "DbtValue"];
	    %beg datetime_ws = ent_debit_set.dbt_value_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_debit_set.dbt_value_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF dbt_book_date OF ent_debit_set = 0
	 AND none IN change_state OF dbt_book_date OF ent_debit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "DbtBook"];
	    %beg datetime_ws = ent_debit_set.dbt_book_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_debit_set.dbt_book_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF cdt_value_date OF ent_credit_set = 0
	 AND none IN change_state OF cdt_value_date OF ent_credit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "CdtValue"];
	    %beg datetime_ws = ent_credit_set.cdt_value_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_credit_set.cdt_value_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF date_time OF cdt_book_date OF ent_credit_set = 0
	 AND none IN change_state OF cdt_book_date OF ent_credit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "PaymentDates" ["Type" "CdtBook"];
	    %beg datetime_ws = ent_credit_set.cdt_book_date.date_time; %end
	    PERFORM f050_compose_datetime THRU f050_compose_datetime_end
	    %XML_compose
		"Status" ent_credit_set.cdt_book_date.change_state "/Status";
	    %XML_compose "/PaymentDates";
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp1 THRU e300_BasicPayment_grp1_end
	END-IF.

	IF bank OF loc_info OF ent_ftr_set  = SPACE
	 AND loc OF loc_info OF ent_ftr_set = SPACE THEN
	    CONTINUE
	ELSE
	    %XML_compose "LocationInf";
	    %XML_compose "IDBank" ent_ftr_set.loc_info.bank "/IDBank";
	    %XML_compose "Location" ent_ftr_set.loc_info.loc "/Location";
	    %XML_compose "/LocationInf";
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp2 THRU e300_BasicPayment_grp2_end
	END-IF.

	IF currency_code OF ent_ftr_set = SPACE
	 AND amount OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "Amount"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND currency_code OF ent_ftr_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_ftr_set.currency_code; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_ftr_set.amount "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF currency_code OF ent_ftr_set = SPACE
	 AND base_amount OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "BaseAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND currency_code OF ent_ftr_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_ftr_set.currency_code; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_ftr_set.base_amount "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF dbt_currency OF ent_debit_set = SPACE
	 AND dbt_amount OF ent_debit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "DbtAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND dbt_currency OF ent_debit_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_debit_set.dbt_currency; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_debit_set.dbt_amount "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF sec_cur OF dbt_acc_2 OF ent_debit_set = SPACE
	 AND sec_amt OF dbt_acc_2 OF ent_debit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "DbtSecAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND sec_cur OF dbt_acc_2 OF ent_debit_set  = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_debit_set.dbt_acc_2.sec_cur; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_debit_set.dbt_acc_2.sec_amt "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF ter_cur OF dbt_acc_3 OF ent_debit_set = SPACE
	 AND ter_amt OF dbt_acc_3 OF ent_debit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "DbtTerAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND ter_cur OF dbt_acc_3 OF ent_debit_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_debit_set.dbt_acc_3.ter_cur; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_debit_set.dbt_acc_3.ter_amt "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF cdt_currency OF ent_credit_set = SPACE
	 AND cdt_amount OF ent_credit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "CdtAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND cdt_currency OF ent_credit_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_credit_set.cdt_currency; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_credit_set.cdt_amount "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF sec_cur OF cdt_acc_2 OF ent_credit_set = SPACE
	 AND sec_amt OF cdt_acc_2 OF ent_credit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "CdtSecAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND sec_cur OF cdt_acc_2 OF ent_credit_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_credit_set.cdt_acc_2.sec_cur; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_credit_set.cdt_acc_2.sec_amt "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF ter_cur OF cdt_acc_3 OF ent_credit_set = SPACE
	 AND ter_amt OF cdt_acc_3 OF ent_credit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "MonetaryAmount" ["Type" "CdtTerAmt"];
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
	     AND ter_cur OF cdt_acc_3 OF ent_credit_set = SPACE THEN
* amount must not be zero
		PERFORM g900_dflt_currency THRU g900_dflt_currency_end
	    ELSE
		%beg currency_code_ws = ent_credit_set.cdt_acc_3.ter_cur; %end
	    END-IF
	    %XML_compose "Currency" currency_code_ws "/Currency";

	    %XML_compose "Amount" ent_credit_set.cdt_acc_3.ter_amt "/Amount";
	    %XML_compose "/MonetaryAmount";
	END-IF.

	IF success_is IN ent_related_amount_seq_status THEN
	    PERFORM f500_RelatedAmt THRU f500_RelatedAmt_end
	END-IF.

	IF success_is IN RLI_wf THEN
	    PERFORM f600_RepetitiveLineInf THRU f600_RepetitiveLineInf_end
	END-IF.

	%beg
	tag_id_ws = ent_ftr_set.reg_id;
	tag_idtype_ws = ent_ftr_set.reg_id_type;
	tag_ws = "PaymentChannel";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp3 THRU e300_BasicPayment_grp3_end
	END-IF.

%^ 127461 Added the If/Then/Else to set the source code to ADM if the amount = 0 and cur is space.
 
        IF (currency_code OF ent_ftr_Set = SPACE
         AND amount OF ent_ftr_set = 0)
	 AND success_is IN xml3p_adm_wf THEN 
	   move "ADM" to str3_ws
	   %XML_compose  "TransactionCode" ent_ftr_set.typ.tran_type "/TransactionCode";
	   %XML_compose "SourceCode" str3_ws "/SourceCode";
	   %XML_compose "MsgType" ent_ftr_set.type_code "/MsgType";
	   %XML_compose "SubType" ent_ftr_set.subtype "/SubType";
	   move spaces to str3_ws
	Else
	   %XML_compose "TransactionCode" ent_ftr_set.typ.tran_type "/TransactionCode";
	   %XML_compose "SourceCode" ent_ftr_set.src_code "/SourceCode";
	   %XML_compose "MsgType" ent_ftr_set.type_code "/MsgType";
	   %XML_compose "SubType" ent_ftr_set.subtype "/SubType";
	End-iF

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp4 THRU e300_BasicPayment_grp4_end
	END-IF.

	%XML_compose
	    "IncomingMsgType" ent_ftr_set.incoming_msgtype "/IncomingMsgType";
	%XML_compose
	    "IncomingFormat" ent_ftr_set.incoming_format "/IncomingFormat";
	%XML_compose "IncomingRef" ent_ftr_set.incoming_ref "/IncomingRef";

	IF success_is IN BR_wf THEN
	    PERFORM f700_BatchRef THRU f700_BatchRef_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp5 THRU e300_BasicPayment_grp5_end
	END-IF.

	IF success_is IN BP_AI_wf THEN
	    PERFORM f800_AccountInf THRU f800_AccountInf_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM e300_BasicPayment_grp6 THRU e300_BasicPayment_grp6_end
	END-IF.

	%XML_compose
	    "BankInf" ["Seq" "1"] ent_debit_set.dbt_bnk_inf1 "/BankInf";
	%XML_compose
	    "BankInf" ["Seq" "2"] ent_debit_set.dbt_bnk_inf2 "/BankInf";
	%XML_compose
	    "BankInf" ["Seq" "3"] ent_debit_set.dbt_bnk_inf3 "/BankInf";
	%XML_compose
	    "BankInf" ["Seq" "4"] ent_debit_set.dbt_bnk_inf4 "/BankInf";
	%XML_compose
	    "BankInf" ["Seq" "5"] ent_debit_set.dbt_bnk_inf5 "/BankInf";
	%XML_compose
	    "BankInf" ["Seq" "6"] ent_debit_set.dbt_bnk_inf6 "/BankInf";
	%XML_compose "BeneficiaryInf" ["Seq" "1"] ent_credit_set.orp_ben_inf1
	    "/BeneficiaryInf";
	%XML_compose "BeneficiaryInf" ["Seq" "2"] ent_credit_set.orp_ben_inf2
	    "/BeneficiaryInf";
	%XML_compose "BeneficiaryInf" ["Seq" "3"] ent_credit_set.orp_ben_inf3
	    "/BeneficiaryInf";
	%XML_compose "BeneficiaryInf" ["Seq" "4"] ent_credit_set.orp_ben_inf4
	    "/BeneficiaryInf";

	IF success_is IN PI_wf THEN
	    PERFORM f900_PartyInf THRU f900_PartyInf_end
	END-IF.

	IF success_is IN PR_wf THEN
	    PERFORM f950_ProcessingRule THRU f950_ProcessingRule_end
	END-IF. 
	
	%XML_compose "/BasicPayment";
	CONTINUE. 

e300_BasicPayment_end.
	EXIT.

e300_BasicPayment_grp1.

	%XML_compose "DraftNumber" ent_debit_set.dbt_draft_num "/DraftNumber";
	CONTINUE.

e300_BasicPayment_grp1_end.
	EXIT.

e300_BasicPayment_grp2.
	IF exch_rate OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "ExchangeRateInf" ["Type" "Exch"];
	    %XML_compose "Rate" ent_ftr_set.exch_rate "/Rate";
	    %XML_compose  "/ExchangeRateInf";
	END-IF.

	IF dbt_exch_rate OF ent_debit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "ExchangeRateInf" ["Type" "Dbt"];
	    %XML_compose "Rate" ent_debit_set.dbt_exch_rate "/Rate";
	    %XML_compose "/ExchangeRateInf";
	END-IF.

	IF cdt_exch_rate OF ent_credit_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "ExchangeRateInf" ["Type" "Cdt"];
	    %XML_compose "Rate" ent_credit_set.cdt_exch_rate "/Rate";
	    %XML_compose "/ExchangeRateInf";
	END-IF.
e300_BasicPayment_grp2_end.
	EXIT.

e300_BasicPayment_grp3.
	IF regulatory_report1 OF ent_credit_set = SPACE THEN
	    CONTINUE
	ELSE
	    %XML_compose "RegulatoryReportDetail";
	    %XML_compose "ID" ["Type" "CdtRpt1"]
		ent_credit_set.regulatory_report1 "/ID";
	    %XML_compose "/RegulatoryReportDetail";
	END-IF.

	IF regulatory_report2 OF ent_credit_set = SPACE THEN
	    CONTINUE
	ELSE
	    %XML_compose "RegulatoryReportDetail";
	    %XML_compose "ID" ["Type" "CdtRpt2"]
		ent_credit_set.regulatory_report2 "/ID";
	    %XML_compose "/RegulatoryReportDetail";
	END-IF.

	IF regulatory_report3 OF ent_credit_set = SPACE THEN
	    CONTINUE
	ELSE
	    %XML_compose "RegulatoryReportDetail";
	    %XML_compose "ID" ["Type" "CdtRpt3"]
		ent_credit_set.regulatory_report3 "/ID";
	    %XML_compose "/RegulatoryReportDetail";
	END-IF.

e300_BasicPayment_grp3_end.
	EXIT.

e300_BasicPayment_grp4.

	%XML_compose "FundsType" ent_ftr_set.typ.funds_type "/FundsType";

	CONTINUE.

e300_BasicPayment_grp4_end.
	EXIT.

e300_BasicPayment_grp5.

	%XML_compose
	    "InstrAdvType" ent_ftr_set.instr_adv_type "/InstrAdvType";

	IF split_ctr OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose "SplitCtr" ent_ftr_set.split_ctr "/SplitCtr";
	END-IF.

	%XML_compose "RprLevel" ent_ftr_set.rpr_level "/RprLevel";
	CONTINUE. 

e300_BasicPayment_grp5_end.
	EXIT.

e300_BasicPayment_grp6.
	%XML_compose "Caller" ent_ftr_set.cal.caller "/Caller";
	%XML_compose "TraderCtrl" ent_ftr_set.trader_ctrl "/TraderCtrl";
	%XML_compose "RiskinessIND" ent_ftr_set.riskiness_ind "/RiskinessIND";
	%XML_compose "DbtChrg" ent_ftr_set.chrg.dbt_chrg "/DbtChrg";
	%XML_compose "CdtChrg" ent_ftr_set.chrg.cdt_chrg "/CdtChrg";
	%XML_compose "Commission" ent_ftr_set.chrg.commission "/Commission";
	%XML_compose "CblChrg" ent_ftr_set.chrg.cbl_charge "/CblChrg";
	%XML_compose "CommMode" ent_ftr_set.chrg.comm_mode "/CommMode";
	%XML_compose "CblMode" ent_ftr_set.chrg.cbl_mode "/CblMode";

	IF orig_exch_rate OF ent_ftr_set = 0 THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"OrigExchRate" ent_ftr_set.orig_exch_rate "/OrigExchRate";
	END-IF.

	%XML_compose "BankOperationCode" ent_ftr_set.bank_operation_code
	    "/BankOperationCode";
	%XML_compose "BoprText" ["Seq" "1"] ent_ftr_set.bopr_text1 "/BoprText";
	%XML_compose "BoprText" ["Seq" "2"] ent_ftr_set.bopr_text2 "/BoprText";
	%XML_compose "BoprText" ["Seq" "3"] ent_ftr_set.bopr_text3 "/BoprText";

	IF nul_is IN msg_state OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "MsgState" ent_ftr_set.msg_state "/MsgState";
	END-IF.

	IF NUL_is IN high_msg_state OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"HighMsgState" ent_ftr_set.high_msg_state "/HighMsgState";
	END-IF.

	IF none IN dbt_chrg_org OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Charges" ["Type" "Dbt"]
		ent_ftr_set.dbt_chrg_org "/Charges";
	END-IF.

	IF none IN cdt_chrg_org OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Charges" ["Type" "Cdt"]
		ent_ftr_set.cdt_chrg_org "/Charges";
	END-IF.

	IF none IN comm_chrg_org OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Charges" ["Type" "Comm"]
		ent_ftr_set.comm_chrg_org "/Charges";
	END-IF.

	IF none IN cbl_chrg_org OF ent_ftr_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Charges" ["Type" "Cbl"]
		ent_ftr_set.cbl_chrg_org "/Charges";
	END-IF.
e300_BasicPayment_grp6_end.
	EXIT.

e400_AuditTrail.
*
* Compose AuditTrail data
* 
* Message  M_wf
*   AuditTrail  AT_wf
*     DstHistory  AT_DH_wf
*     MsgText	AT_MT_wf
*     MsgHistory  AT_MH_wf

	%XML_compose "AuditTrail";

	IF success_is IN AT_DH_wf THEN
	    PERFORM f650_DstHistory THRU f650_DstHistory_end
	END-IF.

	IF success_is IN AT_MT_wf THEN
	    PERFORM f350_MsgText THRU f350_MsgText_end
	END-IF.

	IF success_is IN AT_MH_wf THEN
	    PERFORM f550_MsgHistory THRU f550_MsgHistory_end
	END-IF.

	%XML_compose "/AuditTrail";
	CONTINUE.

e400_AuditTrail_end.
	EXIT.

f000_RepetitiveLineInf.
*
* Determine if there is any RepetitiveLineInf
*
* Message  M_wf
*   BasicPayment  BP_wf
*     RepetitiveLineInf  RLI_wf

	IF success_is IN omit_4_isi2_wf
	OR (
	 idbank OF rptv_id OF ent_ftr_set = SPACE
	 AND idtype OF rptv_id OF ent_ftr_set = SPACE
	 AND idacc OF idkey OF rptv_id OF ent_ftr_set = SPACE
	 AND idadr OF idkey OF rptv_id OF ent_ftr_set = SPACE
	 AND idpad OF idkey OF rptv_id OF ent_ftr_set = SPACE
	 AND idrptv OF rptv_id OF ent_ftr_set = SPACE) THEN
	    %beg RLI_wf failure_is; %end
	ELSE
	    %beg RLI_wf success_is; %end
	END-IF.

f000_RepetitiveLineInf_end.
	EXIT.

f050_compose_datetime.
*
* Do special time formatting: <DateTime><CCYY/MM/DD HH:MM:SS></DateTime>
*
* Input: datetime_ws
*
	IF datetime_ws = 0 THEN
		CONTINUE
    	ELSE
	    %beg
	    compose_lz_ws ^Out( vstr80_ws),
		datetime_ws.slashed_yyyymmdd, " ",
		datetime_ws.hh(^Num<2>), ":", 
		datetime_ws.mn(^Num<2>), ":",
		datetime_ws.ss(^Num<2>), /;
	    %end		
	    %XML_compose "DateTime" vstr80_ws "/DateTime";
	END-IF.

f050_compose_datetime_end.
	EXIT.

f100_BatchRef.
*
* Determine if there is any BatchRef data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     BatchRef  BR_wf

	IF success_is IN omit_4_isi2_wf
	OR (
	 doc_num OF bat_ref OF ent_ftr_set = SPACE
	 AND item_num OF bat_ref OF ent_ftr_set = SPACE
	 AND funds_category OF bat_ref OF ent_ftr_set = SPACE) THEN
	    %beg BR_wf failure_is; %end
	ELSE
	    %beg BR_wf success_is; %end
	END-IF.

f100_BatchRef_end.
	EXIT.

f150_DstHistory.
*
* Message  M_wf
*   AuditTrail  AT_wf
*     DstHistory  AT_DH_wf
*
* Optionally we may have multiple destination sets to compose.
* So we store composable destination sets in a sequence - fi_fi_dst_seq. 

	%beg
%^ failure_is by default unless we change it to success_is
	AT_DH_WF failure_is;

	Break: fi_fi_dst_seq;
	Alloc_temp: fi_fi_dst_seq(Mod);
	%end.

	%ace_is ent_dst_set connected
	IF success_is IN ace_status_wf THEN
* Caller is telling us to XML format for this destination only
	    %beg
	    Break: fi_dst_set;
	    ent_dst_set Equate: fi_dst_set; 
	    %end
* Do the 'check if any to compose' pass now
	    PERFORM g700_DstHistory THRU g700_DstHistory_end
	    IF success_is IN rs_wf THEN
		%beg
		AT_DH_wf success_is; 
%^ Set up our one and only eligible destination set in fi_fi_dst_seq.  This
%^ allows the compose routine to simple process 'all' the fi_fi_dst_seq.
		Alloc_end: fi_fi_dst_seq;
		fi_fi_dst_seq Point: fi_dst_set; 
		%end
	    ELSE
		CONTINUE
* No destination data to compose
	    END-IF
	    GO TO f150_DstHistory_end
	END-IF.

* Caller is telling us to compose all the destination sets.
* So loop through all the destinations, adding that destination set to
* fi_fi_dst_seq if the data qualifies to be composed.

	%beg 
	Break: wrk_main_history;
	Break: wrk_msg_union; 
	ent_msg_history Top: wrk_msg_union;
	wrk_msg_union.msg_history Conn: wrk_main_history;
	First: wrk_main_history;
	%end.

	PERFORM UNTIL failure_is IN wrk_main_history_status
	    IF idname OF qname OF wrk_main_history = "*SUB_HISTORY" THEN
		%beg
		Break: wrk_msg_history;
		Break: fi_dst_set;
		wrk_main_history Conn: wrk_msg_history;
		First: wrk_msg_history; 
		wrk_msg_history Conn: fi_dst_set;
		%end
		PERFORM g700_DstHistory THRU g700_DstHistory_end
		IF success_is IN rs_wf THEN
		    %beg
		    AT_DH_wf success_is; 			
		    Alloc_end: fi_fi_dst_seq;
		    fi_fi_dst_seq Point: fi_dst_set;
		    %end
		ELSE
		    CONTINUE
* just look for another eligible dest set
		END-IF
	    END-IF

	    %beg Next: wrk_main_history; %end
	END-PERFORM. 

f150_DstHistory_end.
	EXIT.			

f200_AccountInf.
*
* Determine if there is any AccountInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     AccountInf  BP_AI_wf
*

	IF success_is IN omit_4_isi2_wf
	OR (
	 nothing_is IN fedfund_type OF acctg OF ent_ftr_set
	 AND fed_days OF acctg OF ent_ftr_set = 0
	 AND interest_rate OF acctg OF ent_ftr_set = 0
	 AND total_amount OF acctg OF ent_ftr_set = 0) THEN
	    %beg BP_AI_wf failure_is; %end
	ELSE	
	    %beg BP_AI_wf success_is; %end
	END-IF. 

f200_AccountInf_end.
	EXIT.

f250_MsgText.
*
* Determine if there is any MsgText.
*
* Message  M_wf
*   AuditTrail  
*     MsgText  AT_MT_wf
*
* Optionally we may have multiple text seqs to compose.
* So we store composable text seqs in a sequence - fi_fi_dst_seq.
* fi_fi_dst_seq is a sequence of text sequences. 
*
	%beg
%^ failure_is by default unless we change it to success_is
	AT_MT_wf failure_is; 

	Break: fi_fi_text_seq; 
	Alloc_temp: fi_fi_text_seq(Mod);

%^ Avoid using caller passed ent_text_seq more than once
	text_ardy_in_seq_wf failure_is;
	%end.

	%ace_is ent_dst_set connected
	IF success_is IN ace_status_wf THEN
* Caller is telling us to XML format for this destination only and to use
* ent_text_seq as our one text sequence
	    %beg
	    Break: fi_text_seq;
	    fi_text_seq(Ftrap);
	    ent_text_seq Equate: fi_text_seq;

%^ Verify that it is not empty
	    First: fi_text_seq; 		
	    %end
	    IF success_is IN fi_text_seq_status THEN
		%beg
		AT_MT_wf success_is; 
%^ Set up our one and only eligible text seq in fi_fi_text_seq.  This
%^ allows the compose routine to simple process 'all' the fi_fi_text_seq.
		Alloc_end: fi_fi_text_seq;
		fi_fi_text_seq Point: fi_text_seq;
		%end
	    ELSE
		CONTINUE
* No text seq data to compose
	    END-IF
	    GO TO f250_MsgText_end
	END-IF.

* Caller is telling us to compose all the destination set's text sequencws.
* So loop through all the destinations, adding that destination set's text
* sequence to fi_fi_text_seq. 

	%beg
	Break: wrk_main_history;
	Break: wrk_msg_union; 
	ent_msg_history Top: wrk_msg_union;
	wrk_msg_union.msg_history Conn: wrk_main_history;
	First: wrk_main_history;
	%end.

	PERFORM UNTIL failure_is IN wrk_main_history_status
	    IF idname OF qname OF wrk_main_history = "*SUB_HISTORY" THEN
		%beg
		Break: wrk_msg_history;
		Break: fi_dst_set;
		wrk_main_history Conn: wrk_msg_history;
		First: wrk_msg_history;
%^ Depending on the type of subhistory, we might need this fi_dst_set
		wrk_msg_history Conn: fi_dst_set;

%^ Set fi_text_seq based on what type of destination
		Next: wrk_msg_history; 
		%end
* I thought what we need is always the 2nd subhistory item but other existing
* code looks further - so we do too. 
		PERFORM UNTIL
		 failure_is IN wrk_msg_history_status
		  OR
		  (idname OF qname OF wrk_msg_history(1:4) = "*CDT"
		   OR "*DBT" OR "*DLV" OR "*SND")
		    %beg Next: wrk_msg_history; %end
		END-PERFORM

		PERFORM g800_set_text_seq THRU g800_set_text_seq_end
		IF success_is IN Rs_wf THEN
		    %beg
		    AT_MT_wf success_is; 
		    Alloc_end: fi_fi_text_seq;
		    fi_fi_text_seq Point: fi_text_seq;
		    %end
		ELSE
		    CONTINUE
* just look for another eligible text seq 
		END-IF
	    END-IF

	    %beg Next: wrk_main_history; %end
	END-PERFORM.

* Not sure if these are exception cases or normal processing.
* If there is no selected text seq yet use the caller passed ent_text_seq.
* And if the caller passed ent_text_seq is blank, use
* ent_msg_union.ent_text_seq.

	%beg First: fi_fi_text_seq; %end.
	IF failure_is IN fi_fi_text_seq_status THEN
* empty - so add ent_text_seq
	    %beg
	    Break: fi_text_seq;
	    ent_text_seq Equate: fi_text_seq;
	    fi_text_seq(Ftrap); 
	    First: fi_text_seq;
	    %end
	    IF success_is IN fi_text_seq_status THEN
		%beg
		AT_MT_wf success_is; 
		Alloc_end: fi_fi_text_seq;
		fi_fi_text_seq Point: fi_text_seq;
		%end
		GO TO f250_MsgText_end
	    ELSE
		CONTINUE
	    END-IF
	ELSE
* Already have some text
	    GO TO f250_MsgText_end
	END-IF.

* No text yet, try ent_msg_union.txt as a last effort.
	%beg
	Break: fi_text_seq; 
	ent_msg_union.txt Conn: fi_text_seq;
	First: fi_text_seq; 
	%end.
	IF success_is IN fi_text_seq_status THEN
	    %beg
	    AT_MT_wf success_is; 
	    Alloc_end: fi_fi_text_seq;
	    fi_fi_text_seq Point: fi_text_seq;
	    %end
	END-IF. 

f250_MsgText_end.
	EXIT.

f300_PartyInf.
*
* Determine if there is any PartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	OriginatingPartyInf
*	  BasicPartyInf  OPI_BPI_wf
*       OrderingBankInf
*         BasicPartyInf  OBI_BPI_wf
*       InstructingBankInf
*         BasicPartyInf  IBI_BPI_wf
*       SendingBankInf
*         BasicPartyInf  SBI_BPI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	CreditPartyInf  CPI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	IntermediaryBankInf IB1  IB1_wf
*	BeneficiaryBankInf  BBI_wf
*	BeneficiaryPartyInf  BPI_wf

	PERFORM g000_OriginatingPartyInf THRU g000_OriginatingPartyInf_end.
	PERFORM g025_OrderingBankInf THRU g025_OrderingBankInf_end.
	PERFORM g050_InstructingBankInf THRU g050_InstructingBankInf_end.
	PERFORM g075_SendingBankInf THRU g075_SendingBankInf_end.
	PERFORM g100_DebitPartyInf THRU g100_DebitPartyInf_end.
	PERFORM g125_CreditPartyInf THRU g125_CreditPartyInf_end.
	PERFORM g150_IntermediaryBankInf_K THRU
	    g150_IntermediaryBankInf_K_end.
	PERFORM g175_IntermediaryBankInf_1 THRU
	    g175_IntermediaryBankInf_1_end.
	PERFORM g200_BeneficiaryBankInf THRU g200_BeneficiaryBankInf_end.
	PERFORM g225_BeneficiaryPartyInf THRU g225_BeneficiaryPartyInf_end.

	IF failure_is IN OPI_BPI_wf
	 AND failure_is IN OBI_BPI_wf
	 AND failure_is IN IBI_BPI_wf
	 AND failure_is IN SBI_BPI_wf
	 AND failure_is IN DPI_API_wf
	 AND failure_is IN CPI_wf
	 AND failure_is IN IBK_wf
	 AND failure_is IN IB1_wf
	 AND failure_is IN BBI_wf
	 AND failure_is IN BPI_wf THEN
	    %beg PI_wf failure_is; %end
	ELSE
	    %beg PI_wf success_is; %end
	END-IF. 

f300_PartyInf_end.
	EXIT.

f350_MsgText.
*
* Compose AuditTrail/MsgText data
*
* Message  M_wf
*   AuditTrail  
*     MsgText  AT_MT_wf
*
* There can be multiple text sequences.
* The text sequences are stored in fi_fi_text_seq (the sequence of text
* sequences). 

* This First is guaranteed to succeed or we would not be here (AT_MT_wf)
	%beg First: fi_fi_text_seq; %end.

	PERFORM UNTIL failure_is IN fi_fi_text_seq_status 

	    %beg
	    Break: fi_text_seq;
	    fi_fi_text_seq Conn: fi_text_seq;
%^ This First is guaranteed to succeed or we would not be here for only non
%^ empty text seqs were put into fi_fi_text_seq
	    First: fi_text_seq; 
	    %end

	    %XML_compose "MsgText";

	    PERFORM UNTIL failure_is IN fi_text_seq_status
		%XML_compose "Text" fi_text_seq.txt "/Text";
		%beg Next: fi_text_seq; %end
	    END-PERFORM

	    %XML_compose "/MsgText";

	    %beg Next: fi_fi_text_seq; %end

	END-PERFORM.

f350_MsgText_end.
	EXIT.

f400_ProcessingRule.
*
* Determine if there is any ProcessingRule data for certain callers
*
* Message  M_wf
*   BasicPayment  BP_wf
*     ProcessingRule  PR_wf

* Initialize for  config calls
	%beg
	Cfg_union_key_ws (.Idname = "FORMAT_IFML_PRULES",
				  .Idprod = "MTS",
				  .Idbank = null,
				  .Idloc = null,
				  .Idcust = null);	

	Cfg_item_key_ws = "IFML_PRULE_SELECTION:";
	Cfg_seq_ordinal_ws = <0>;
	Cfg_item_type_ws = "STR(130)";

	%end.

* Start at the top with the config table
	%beg next_state_wf get_config_is; %end.
	PERFORM g500_get_prule_with_exec THRU g500_get_prule_with_exec_end.

	IF success_is IN prule_found_wf THEN
* found 1.  Record the fact that we found 1 and retain it for the 1st
* processing rule compose done later.
	    %beg PR_wf success_is; %end
	ELSE	
	    %beg PR_wf failure_is; %end
	END-IF. 

f400_ProcessingRule_end.
	EXIT.

f450_MsgHistory.
*
* Message  M_wf
*   AuditTrail  AT_wf
*     MsgHistory  AT_MH_wf

	IF success_is IN omit_4_stp_wf THEN
	    %beg AT_MH_wf failure_is; %end
	    GO TO f450_MsgHistory_end
	END-IF.

* Hunt for a history memo with text.
* If found, leave it for immediate access by the compose routine.

	%beg
	Break: fi_msg_history; 
	ent_msg_history Equate: fi_msg_history(Nomod);
	First: fi_msg_history;
	%end.

	PERFORM UNTIL failure_is IN fi_msg_history_status
	    IF memo OF fi_msg_history > SPACE THEN
* Got one - keep it ready to use by the compse routine
		%beg AT_MH_wf success_is; %end
		GO TO f450_MsgHistory_end
	    END-IF
	%beg Next: fi_msg_history; %end

	END-PERFORM.

* None found
	%beg
	Break: fi_msg_history;
	AT_MH_wf failure_is;
	%end.

f450_MsgHistory_end.
	EXIT.

f500_RelatedAmt.
*
* Compose the RelatedAmt MonetaryAmount data.
*
* Already positioned to the first one

	%beg which_seq_ws = <1>; %end.
	PERFORM UNTIL failure_is IN ent_related_amount_seq_status
	    IF (curr OF ent_related_amount_seq = SPACE
	     AND amount OF ent_related_amount_seq = 0
	     AND (memo OF ent_related_amount_seq = SPACE
* don't consider it if we don't format it
                  OR success_is IN omit_4_isi2_wf))
* ignore if no amt_codeword - would cause us problems below
	    OR amt_codeword OF ent_related_amount_seq = SPACE THEN
		CONTINUE
	    ELSE
		%XML_compose "MonetaryAmount" ["Type" "RelatedAmt"];
		%XML_compose
		    "SubType" ent_related_amount_seq.amt_codeword "/SubType";
		%XML_compose "RelatedAmtSeq" which_seq_ws "/RelatedAmtSeq";
* 140417
	    IF success_is IN formatting_isi2_wf
* isi2-xml only
		 AND curr OF ent_related_amount_seq = SPACE THEN
* amount must not be zero
		    PERFORM g900_dflt_currency THRU g900_dflt_currency_end
		ELSE
		    %beg currency_code_ws = ent_related_amount_seq.curr; %end
		END-IF
		%XML_compose "Currency" currency_code_ws "/Currency";

		%XML_compose "Amount" ent_related_amount_seq.amount "/Amount";
		IF failure_is IN omit_4_isi2_wf THEN
		    %XML_compose "Memo" ent_related_amount_seq.memo "/Memo";
		END-IF
		%XML_compose "/MonetaryAmount";
	    END-IF
	    %beg Next: ent_related_amount_seq; %end
	    Add +1 TO which_seq_ws
	END-PERFORM. 

f500_RelatedAmt_end.
	EXIT.

f550_MsgHistory.
*
* Compose MsgHistory data
* 
* Message  M_wf
*   AuditTrail  AT_wf
*     MsgHistory  AT_MH_wf
*

* First item set by 'checker routine'.

	%XML_compose "MsgHistory";

	PERFORM WITH TEST AFTER until failure_is IN fi_msg_history_status
* Note the subtile skipping of fi_msg_history items without memos (handled
* in the macro)
	    %XML_compose "InformationalData" fi_msg_history.memo
		"/InformationalData";
	    %beg Next: fi_msg_history; %end		
	END-PERFORM.

	%XML_compose "/MsgHistory";

	CONTINUE. 

f550_MsgHistory_end.
	EXIT.

f600_RepetitiveLineInf.
*
* Compose the RepetitiveLineInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     RepetitiveLineInf  RLI_wf
*
	%XML_compose "RepetitiveLineInf";

	%XML_compose "IDBank" ent_ftr_set.rptv_id.idbank "/IDBank";

	%XML_compose "ID" ent_ftr_set.rptv_id.idtype "/ID";

	%beg vstr132_ws = ent_ftr_set.rptv_id.Idkey.Idacc; %end
	%XML_compose "IDAcc" vstr132_ws "/IDAcc";

	%beg vstr132_ws = ent_ftr_set.rptv_id.idkey.idadr; %end
	%XML_compose "IDAdr" vstr132_ws "/IDAdr";

	%beg vstr132_ws = ent_ftr_set.rptv_id.idkey.idpad; %end
	%XML_compose "IDPad" vstr132_ws "/IDPad";

	%XML_compose
	    "RepetitiveLineNo" ent_ftr_set.rptv_id.idrptv "/RepetitiveLineNo";

	%XML_compose "/RepetitiveLineInf";
	CONTINUE.

f600_RepetitiveLineInf_end.
	EXIT.

f650_DstHistory.
*
* Compose DstHistory data
* 
* Message  M_wf
*   AuditTrail  AT_wf
*     DstHistory  AT_DH_wf
*
* There can be multiple destination sets to compose.
* The destination sets to compose  are stored in fi_fi_dst_seq
*

* The First is guaranteed to succeed or we would not be here (AT_DH_wf)
	%beg First: fi_fi_dst_seq; %end.

	PERFORM UNTIL failure_is IN fi_fi_dst_seq_status

	    %beg
	    Break: fi_dst_set;
	    fi_fi_dst_seq Conn: fi_dst_set; 
	    %end

	    %XML_compose "DstHistory";

	    IF success_is IN omit_4_stp_wf THEN
		CONTINUE
	    ELSE
		PERFORM F650_DstHistory_grp1 THRU F650_DstHistory_grp1_end
	    END-IF

	    %XML_compose "DstRouteID" fi_dst_set.dst_route_id "/DstRouteID";

	    %XML_compose "/DstHistory";

	    %beg Next: fi_fi_dst_seq; %end

	END-PERFORM.

f650_DstHistory_end.
	EXIT.

F650_DstHistory_grp1.
	%XML_compose "DstOrdinal" fi_dst_set.dst_ordinal "/DstOrdinal";
	%XML_compose "AdrBankID" fi_dst_set.dst_adr_bnk_id "/AdrBankID";
	%XML_compose "DstType" fi_dst_set.dst_typ "/DstType";
	%XML_compose "Name" fi_dst_set.dst_name1 "/Name";
	%XML_compose "AddressInf" fi_dst_set.dst_cab2 "/AddressInf";
	%XML_compose "AddressInf" fi_dst_set.dst_cab3 "/AddressInf";
	%XML_compose "AddressInf" fi_dst_set.dst_cab4 "/AddressInf";
	%XML_compose "Country" fi_dst_set.dst_country "/Country";

	CONTINUE. 

F650_DstHistory_grp1_end.
	EXIT.

f700_BatchRef.
*
* Compose the BatchRef data
*
* Message  M_wf
*   BasicPayment  BP_wf
*     BatchRef  BR_wf
*
	%XML_compose "BatchRef";

	%XML_compose "DocNumber" ent_ftr_set.bat_ref.doc_num "/DocNumber";
	%XML_compose "ItemNumber" ent_ftr_set.bat_ref.item_num "/ItemNumber";
	%XML_compose "FundsCategory" ent_ftr_set.bat_ref.funds_category
	    "/FundsCategory";

	%XML_compose "/BatchRef";
	CONTINUE.

f700_BatchRef_end.
	EXIT.

f800_AccountInf.
*
* Compose the AccountInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     AccountInf  BP_AI_wf
*
	%XML_compose "AccountInf";

	%XML_compose
	    "FedFundType" ent_ftr_set.acctg.fedfund_type "/FedFundType";
	%XML_compose "FedDays" ent_ftr_set.acctg.fed_days "/FedDays";
	%XML_compose
	    "InterestRate" ent_ftr_set.acctg.interest_rate "/InterestRate";
	%XML_compose "TotalAmt" ent_ftr_set.acctg.total_amount "/TotalAmt";

	%XML_compose "/AccountInf";
	CONTINUE.

f800_AccountInf_end.
	EXIT.

f900_PartyInf.
*
* Compose PartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	OriginatingPartyInf
*	  BasicPartyInf  OPI_BPI_wf
*       OrderingBankInf
*         BasicPartyInf  OBI_BPI_wf
*       InstructingBankInf
*         BasicPartyInf  IBI_BPI_wf
*       SendingBankInf
*         BasicPartyInf  SBI_BPI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	CreditPartyInf  CPI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	IntermediaryBankInf IB1  IB1_wf
*	BeneficiaryBankInf  BBI_wf
*	BeneficiaryPartyInf  BPI_wf
*
	%XML_compose "PartyInf";

	IF success_is IN OPI_BPI_wf THEN
	    PERFORM g250_OriginatingPartyInf THRU g250_OriginatingPartyInf_end
	END-IF.

	IF success_is IN OBI_BPI_wf THEN
	    PERFORM g275_OrderingBankInf THRU g275_OrderingBankInf_end
	END-IF.

	IF success_is IN IBI_BPI_wf THEN
	    PERFORM g300_InstructingBankInf THRU g300_InstructingBankInf_end
	END-IF.

	IF success_is IN SBI_BPI_wf THEN
	    PERFORM g325_SendingBankInf THRU g325_SendingBankInf_end
	END-IF.

	IF success_is IN DPI_API_wf THEN
	    PERFORM g350_DebitPartyInf THRU g350_DebitPartyInf_end
	END-IF.

	IF success_is IN CPI_wf THEN
	    PERFORM g375_CreditPartyInf THRU g375_CreditPartyInf_end
	END-IF.

	IF success_is IN IBK_wf THEN
	    PERFORM g400_IntermediaryBankInf_K THRU
		g400_IntermediaryBankInf_K_end
	END-IF.

	IF success_is IN IB1_wf THEN
	    PERFORM g425_IntermediaryBankInf_1 THRU
		g425_IntermediaryBankInf_1_end
	END-IF.

	IF success_is IN BBI_wf THEN
	    PERFORM g450_BeneficiaryBankInf THRU g450_BeneficiaryBankInf_end
	END-IF.

	IF success_is IN BPI_wf THEN
	    PERFORM g475_BeneficiaryPartyInf THRU g475_BeneficiaryPartyInf_end
	END-IF. 

	%XML_compose "/PartyInf";
	CONTINUE.

f900_PartyInf_end.
	EXIT.

f950_ProcessingRule.
*
* Compose the ProcessingRule data for certain callers.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     ProcessingRule  PR_wf
*

* First item already positioned back when we checked if there were any

	%beg first_prule_wf success_is; %end.
	PERFORM WITH TEST AFTER UNTIL failure_is IN prule_found_wf
	    IF success_is IN all_execs_option_wf THEN
		PERFORM g600_compose_all_execs THRU g600_compose_all_execs_end
	    ELSE
		PERFORM h175_compose_prule THRU h175_compose_prule_end
	    END-IF
	    PERFORM g500_get_prule_with_exec THRU g500_get_prule_with_exec_end
	END-PERFORM.

* Finally finish things off
	%XML_compose "/ExecutionParm";
	%XML_compose "/ProcessingRule";
	CONTINUE.

f950_ProcessingRule_end.
	EXIT.

g000_OriginatingPartyInf.
*
* Determine if there is any OriginatingPartyInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	OriginatingPartyInf
*	  BasicPartyInf  OPI_BPI_wf

	IF orp_adr_bnk_id OF ent_debit_set = SPACE
* ignore orp_idtype without a orp_id
	 AND orp_id OF orp OF ent_debit_set = SPACE
	 AND orp_bei_flag OF ent_debit_set = SPACE
	 AND orp_name1 OF ent_debit_set = SPACE
	 AND orp_name2 OF ent_debit_set = SPACE
	 AND orp_name3 OF ent_debit_set = SPACE
	 AND orp_name4 OF ent_debit_set = SPACE
	 AND orp_state OF ent_debit_set = SPACE
	 AND orp_postal_code OF ent_debit_set = SPACE
	 AND orp_res_country OF ent_debit_set = SPACE
	 AND orp_ref_num OF ent_debit_set = SPACE THEN
	    %beg OPI_BPI_wf failure_is; %end
	ELSE
	    %beg OPI_BPI_wf success_is; %end
	END-IF. 

g000_OriginatingPartyInf_end.
	EXIT.

g025_OrderingBankInf.
*
* Determine if there is an OrderingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       OrderingBankInf
*         BasicPartyInf  OBI_BPI_wf

	IF obk_adr_bnk_id OF ent_debit_set = SPACE
* ignore obk_idtype without a obk_id
	 AND obk_id OF obk OF ent_debit_set = SPACE
	 AND obk_bei_flag OF ent_debit_set = SPACE
	 AND obk_name1 OF ent_debit_set = SPACE
	 AND obk_name2 OF ent_debit_set = SPACE
	 AND obk_name3 OF ent_debit_set = SPACE
	 AND obk_name4 OF ent_debit_set = SPACE
	 AND obk_state OF ent_debit_set = SPACE
	 AND obk_postal_code OF ent_debit_set = SPACE
	 AND obk_res_country OF ent_debit_set = SPACE
	 AND obk_ref_num OF ent_debit_set = SPACE THEN
	    %beg OBI_BPI_wf failure_is; %end
	ELSE
	    %beg OBI_BPI_wf success_is; %end
	END-IF. 
	
g025_OrderingBankInf_end.
	EXIT.

g050_InstructingBankInf.
*
* Determine if there is any InstructingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       InstructingBankInf
*         BasicPartyInf  IBI_BPI_wf

	IF ins_adr_bnk_id OF ent_debit_set = SPACE
* ignore ins_idtype without an ins_id
	 AND ins_id OF ins OF ent_debit_set = SPACE
	 AND ins_bei_flag OF ent_debit_set = SPACE
	 AND ins_name1 OF ent_debit_set = SPACE
	 AND ins_name2 OF ent_debit_set = SPACE
	 AND ins_name3 OF ent_debit_set = SPACE
	 AND ins_name4 OF ent_debit_set = SPACE
	 AND ins_state OF ent_debit_set = SPACE
	 AND ins_postal_code OF ent_debit_set = SPACE
	 AND ins_res_country OF ent_debit_set = SPACE
	 AND ins_ref_num OF ent_debit_set = SPACE THEN
	    %beg IBI_BPI_wf failure_is; %end
	ELSE
	    %beg IBI_BPI_wf success_is; %end
	END-IF. 

g050_InstructingBankInf_end.
	EXIT.

g075_SendingBankInf.
*
* Determine if there is any SendingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       SendingBankInf
*         BasicPartyInf  SBI_BPI_wf

	IF sbk_adr_bnk_id OF ent_debit_set = SPACE
* ignore sbk_idtype without a sbk_id 
	 AND sbk_id OF sbk OF ent_debit_set = SPACE
	 AND sbk_bei_flag OF ent_debit_set = SPACE
	 AND sbk_name1 OF ent_debit_set = SPACE
	 AND sbk_name2 OF ent_debit_set = SPACE
	 AND sbk_name3 OF ent_debit_set = SPACE
	 AND sbk_name4 OF ent_debit_set = SPACE
	 AND sbk_state OF ent_debit_set = SPACE
	 AND sbk_postal_code OF ent_debit_set = SPACE
	 AND sbk_res_country OF ent_debit_set = SPACE
	 AND sbk_ref_num OF ent_debit_set = SPACE THEN
	    %beg SBI_BPI_wf failure_is; %end
	ELSE
	    %beg SBI_BPI_wf success_is; %end
	END-IF. 

g075_SendingBankInf_end.
	EXIT.

g100_DebitPartyInf.
*
* Determine if there is any DebitPartyInf/AccountPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    AcctIDInf  DPI_API_AIDI_wf
*           SecondaryID  DPI_API_SID_wf
*	    TertiaryID  DPI_API_TID_wf
*	    ConcentrationAccount  DPI_API_CA_wf

	PERFORM h000_AcctIDInf THRU h000_AcctIDInf_end.
	PERFORM h050_SecondaryID THRU h050_SecondaryID_end.
	PERFORM h100_TertiaryID THRU h100_TertiaryID_end.
	PERFORM h150_ConcentrationAccount THRU h150_ConcentrationAccount_end.

* Handle the xml formatting exclusion requirements of the caller
	IF success_is IN omit_4_isi2_wf
	OR (
	 dbt_recon_ref OF ent_debit_set = SPACE
	 AND dbt_acc_class OF ent_debit_set = SPACE
	 AND dbt_acc_parent_code OF ent_debit_set = SPACE
	 AND dbt_acc_prod_codes OF ent_debit_set = SPACE) THEN
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 dbt_adv_typ OF ent_debit_set = SPACE
	 AND dbt_department OF ent_debit_set = SPACE
	 AND dbt_sys_of_rec OF ent_debit_set = SPACE) THEN
	    %beg grp_2_ws = <0>; %end
	ELSE
	    %beg grp_2_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 dbt_fee_code OF ent_debit_set = SPACE
	 AND dbt_sec_sub_acct OF ent_debit_set = SPACE
* ignore idtype without an idkey
	 AND idkey OF dbt_wir_key OF ent_debit_set = SPACE) THEN
	    %beg grp_3_ws = <0>; %end
	ELSE
	    %beg grp_3_ws = <1>; %end
	END-IF.

	IF dbt_adr_bnk_id OF ent_debit_set = SPACE
	 AND failure_is IN DPI_API_AIDI_wf
	 AND grp_1_ws = 0
	 AND failure_is IN DPI_API_SID_wf
	 AND failure_is IN DPI_API_TID_wf
	 AND dbt_name1 OF ent_debit_set = SPACE
	 AND dbt_name2 OF ent_debit_set = SPACE
	 AND dbt_name3 OF ent_debit_set = SPACE
	 AND dbt_name4 OF ent_debit_set = SPACE
	 AND (dbt_adr_class OF ent_debit_set = SPACE
	      OR success_is IN omit_4_isi2_wf)
	 AND dbt_adr_city OF ent_debit_set = SPACE
	 AND dbt_adr_country OF ent_debit_set = SPACE
	 AND dbt_state OF ent_debit_set = SPACE
	 AND dbt_postal_code OF ent_debit_set = SPACE
	 AND dbt_res_country OF ent_debit_set = SPACE
	 AND dbt_acc_city OF ent_debit_set = SPACE
	 AND dbt_acc_country OF ent_debit_set = SPACE
	 AND dbt_ref_num OF ent_debit_set = SPACE
	 AND grp_2_ws = 0
	 AND failure_is IN DPI_API_CA_wf
	 AND grp_3_ws = 0 THEN
	    %beg DPI_API_wf failure_is; %end
	ELSE
	    %beg DPI_API_wf success_is; %end
	END-IF. 
	
g100_DebitPartyInf_end.
	EXIT.

g125_CreditPartyInf.
*
* Determine if there is any CreditPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf

	PERFORM h200_AccountPartyInf THRU h200_AccountPartyInf_end.

	IF failure_is IN CPI_API_wf
	 AND (cdt_location OF ent_credit_set = SPACE
	      OR success_is IN omit_4_isi2_wf) THEN
	    %beg CPI_wf failure_is; %end
	ELSE
	    %beg CPI_wf success_is; %end
	END-IF.

g125_CreditPartyInf_end.
	EXIT.

g150_IntermediaryBankInf_K.
*
* Determine if there is any IntermediaryBankInf IBK data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	  BasicPartyInf  IBK_BPI_wf

	PERFORM h250_BasicPartyInf THRU h250_BasicPartyInf_end.

	IF failure_is IN IBK_BPI_wf
	 AND ibk_secwir OF ibk OF ent_credit_set = SPACE THEN
	    %beg IBK_wf failure_is; %end
	ELSE
	    %beg IBK_wf success_is; %end
	END-IF.

g150_IntermediaryBankInf_K_end.
	EXIT.

g175_IntermediaryBankInf_1.
*
* Determine if there is any IntermediaryBankInf IB1 data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IB1  IB1_wf
*	  BasicPartyInf  IB1_BPI_wf

	PERFORM h300_BasicPartyInf THRU h300_BasicPartyInf_end.

	IF failure_is IN IB1_BPI_wf
	 AND (ib1_secwir OF ib1 OF ent_credit_set = SPACE) THEN
	    %beg IB1_wf failure_is; %end
	ELSE
	    %beg IB1_wf success_is; %end
	END-IF.

g175_IntermediaryBankInf_1_end.
	EXIT.

g200_BeneficiaryBankInf.
*
* Determine if there is any BeneficiaryBankInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryBankInf  BBI_wf
*         BasicPartyInf  BBI_BPI_wf

	PERFORM h350_BasicPartyInf THRU h350_BasicPartyInf_end.

	IF failure_is IN BBI_BPI_wf
	 AND bbk_secwir OF bbk OF ent_credit_set = SPACE THEN
	    %beg BBI_wf failure_is; %end
	ELSE
	    %beg BBI_wf success_is; %end
	END-IF. 

g200_BeneficiaryBankInf_end.
	EXIT.

g225_BeneficiaryPartyInf.
*
* Determine if there is any BeneficiaryPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryPartyInf  BPI_wf
*	  BasicPartyInf  BPI_BPI_wf

	PERFORM h400_BasicPartyInf THRU h400_BasicPartyInf_end.

* Handle the xml formatting exclusion requirements of the caller
	IF success_is IN omit_4_isi2_wf
	OR (
	 charge_flg OF bnp OF ent_credit_set = SPACE) THEN
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

	IF failure_is IN BPI_BPI_wf
	 AND grp_1_ws = 0
	 AND bnp_bnk_flg OF ent_credit_set = SPACE
	 AND bnp_mailing_country OF ent_credit_set = SPACE THEN
	    %beg BPI_wf failure_is; %end
	ELSE
	    %beg BPI_wf success_is; %end
	END-IF.

g225_BeneficiaryPartyInf_end.
	EXIT.

g250_OriginatingPartyInf.
*
* Compose OriginatingPartyInf/BasicPartyInf data
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	OriginatingPartyInf
*	  BasicPartyInf  OPI_BPI_wf
*
	%XML_compose "OriginatingPartyInf";	
	%XML_compose "BasicPartyInf";

	%XML_compose "AdrBankID" ent_debit_set.orp_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_debit_set.orp.orp_id;
	tag_idtype_ws = ent_debit_set.orp.orp_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_debit_set.orp_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"] ent_debit_set.orp_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"] ent_debit_set.orp_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"] ent_debit_set.orp_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"] ent_debit_set.orp_name4 "/AddressInf";
	%XML_compose "State" ent_debit_set.orp_state "/State";
	%XML_compose
		"PostalCode" ent_debit_set.orp_postal_code "/PostalCode";
	%XML_compose "Country" ent_debit_set.orp_res_country "/Country";
	%XML_compose "Reference" ent_debit_set.orp_ref_num "/Reference";

	%XML_compose "/BasicPartyInf";
	%XML_compose "/OriginatingPartyInf";
	CONTINUE.

g250_OriginatingPartyInf_end.
	EXIT.

g275_OrderingBankInf.
*
* Compose OrderingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       OrderingBankInf
*         BasicPartyInf  OBI_BPI_wf
*
	%XML_compose "OrderingBankInf";
	%XML_compose "BasicPartyInf";

	%XML_compose "AdrBankID" ent_debit_set.obk_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_debit_set.obk.obk_id;
	tag_idtype_ws = ent_debit_set.obk.obk_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_debit_set.obk_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"]  ent_debit_set.obk_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"]  ent_debit_set.obk_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"]  ent_debit_set.obk_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"]  ent_debit_set.obk_name4 "/AddressInf";
	%XML_compose "State" ent_debit_set.obk_state "/State";
	%XML_compose "PostalCode" ent_debit_set.obk_postal_code "/PostalCode";
	%XML_compose "Country" ent_debit_set.obk_res_country "/Country";
	%XML_compose "Reference" ent_debit_set.obk_ref_num "/Reference";

	%XML_compose "/BasicPartyInf";
	%XML_compose "/OrderingBankInf";
	CONTINUE. 

g275_OrderingBankInf_end.
	EXIT.

g300_InstructingBankInf.
*
* Compose the InstructingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       InstructingBankInf
*         BasicPartyInf  IBI_BPI_wf
*
	%XML_compose "InstructingBankInf";
	%XML_compose "BasicPartyInf";

	%XML_compose "AdrBankID" ent_debit_set.ins_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_debit_set.ins.ins_id;
	tag_idtype_ws = ent_debit_set.ins.ins_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_debit_set.ins_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"] ent_debit_set.ins_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"] ent_debit_set.ins_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"] ent_debit_set.ins_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"] ent_debit_set.ins_name4 "/AddressInf";
	%XML_compose "State" ent_debit_set.ins_state "/State";
	%XML_compose "PostalCode" ent_debit_set.ins_postal_code "/PostalCode";
	%XML_compose "Country" ent_debit_set.ins_res_country "/Country";
	%XML_compose "Reference" ent_debit_set.ins_ref_num "/Reference";

	%XML_compose "/BasicPartyInf";
	%XML_compose "/InstructingBankInf";
	CONTINUE.

g300_InstructingBankInf_end.
	EXIT.

g325_SendingBankInf.
*
* Compose the SendingBankInf/BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*       SendingBankInf
*         BasicPartyInf  SBI_BPI_wf
*
	%XML_compose "SendingBankInf";
	%XML_compose "BasicPartyInf";

	%XML_compose "AdrBankID" ent_debit_set.sbk_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_debit_set.sbk.sbk_id;
	tag_idtype_ws = ent_debit_set.sbk.sbk_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_debit_set.sbk_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"] ent_debit_set.sbk_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"] ent_debit_set.sbk_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"] ent_debit_set.sbk_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"] ent_debit_set.sbk_name4 "/AddressInf";
	%XML_compose "State" ent_debit_set.sbk_state "/State";
	%XML_compose "PostalCode" ent_debit_set.sbk_postal_code "/PostalCode";
	%XML_compose "Country" ent_debit_set.sbk_res_country "/Country";
	%XML_compose "Reference" ent_debit_set.sbk_ref_num "/Reference";

	%XML_compose "/BasicPartyInf";
	%XML_compose "/SendingBankInf";
	CONTINUE. 

g325_SendingBankInf_end.
	EXIT.

g350_DebitPartyInf.
*
* Compose DebitPartyInf/AccountPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    AcctIDInf  DPI_API_AIDI_wf
*           SecondaryID  DPI_API_SID_wf
*	    TertiaryID  DPI_API_TID_wf
*	    ConcentrationAccount  DPI_API_CA_wf
*
	%XML_compose "DebitPartyInf";
	%XML_compose "AccountPartyInf";

	%XML_compose "AdrBankID" ent_debit_set.dbt_adr_bnk_id "/AdrBankID";

	IF success_is IN DPI_API_AIDI_wf THEN
	    PERFORM h450_AcctIDInf THRU h450_AcctIDInf_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM g350_DebitPartyInf_grp1 THRU g350_DebitPartyInf_grp1_end
	END-IF.

	IF success_is IN DPI_API_SID_wf THEN
	    PERFORM h500_SecondaryID THRU h500_SecondaryID_end
	END-IF.

	IF success_is IN DPI_API_TID_wf THEN
	    PERFORM h550_TertiaryID THRU h550_TertiaryID_end
	END-IF.

	%XML_compose
	    "AddressInf" ["Seq" "1"] ent_debit_set.dbt_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"] ent_debit_set.dbt_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"] ent_debit_set.dbt_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"] ent_debit_set.dbt_name4 "/AddressInf";

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"AddressClass" ent_debit_set.dbt_adr_class "/AddressClass";
	END-IF.

	%XML_compose "AddressCity" ent_debit_set.dbt_adr_city "/AddressCity";
	%XML_compose
	    "AddressCountry" ent_debit_set.dbt_adr_country "/AddressCountry";
	%XML_compose "State" ent_debit_set.dbt_state "/State";
	%XML_compose "PostalCode" ent_debit_set.dbt_postal_code "/PostalCode";
	%XML_compose "ResidenceCountry" ent_debit_set.dbt_res_country
	    "/ResidenceCountry";
	%XML_compose "AccountCity" ent_debit_set.dbt_acc_city "/AccountCity";
	%XML_compose
	    "AccountCountry" ent_debit_set.dbt_acc_country "/AccountCountry";
	%XML_compose "Reference" ent_debit_set.dbt_ref_num "/Reference";

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM g350_DebitPartyInf_grp2 THRU g350_DebitPartyInf_grp2_end
	END-IF.

	IF success_is IN DPI_API_CA_wf THEN
	    PERFORM h600_ConcentrationAccount THRU
		h600_ConcentrationAccount_end
	END-IF.

	IF (success_is IN omit_4_isi2_wf
	 OR idkey OF dbt_wir_key OF ent_debit_set = SPACE) THEN
	    CONTINUE
	ELSE
	    %XML_compose "WireKey";
	    %XML_compose "AcctIDInf";

	    %beg
	    tag_id_ws = ent_debit_set.dbt_wir_key.idkey;
	    tag_idtype_ws = ent_debit_set.dbt_wir_key.idtype;
	    tag_ws = "ID";
	    %end
	    PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end

	    %XML_compose "/AcctIDInf";
	    %XML_compose "/WireKey";
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE  
	ELSE
	    PERFORM g350_DebitPartyInf_grp4 THRU g350_DebitPartyInf_grp4_end
	END-IF.

	%XML_compose "/AccountPartyInf";
	%XML_compose "/DebitPartyInf";
	CONTINUE.

g350_DebitPartyInf_end.
	EXIT.

g350_DebitPartyInf_grp1.
	%XML_compose "ReconRef" ent_debit_set.dbt_recon_ref "/ReconRef";
	%XML_compose "Class" ent_debit_set.dbt_acc_class "/Class";
	%XML_compose
	    "ParentCode" ent_debit_set.dbt_acc_parent_code "/ParentCode";
	%XML_compose
	    "ProductCode" ent_debit_set.dbt_acc_prod_codes "/ProductCode";
	CONTINUE.
g350_DebitPartyInf_grp1_end.
	EXIT.

g350_DebitPartyInf_grp2.
	%XML_compose "AdviceType" ent_debit_set.dbt_adv_typ "/AdviceType";
	%XML_compose "Department" ent_debit_set.dbt_department "/Department";
	%XML_compose
	    "SystemofRecord" ent_debit_set.dbt_sys_of_rec "/SystemofRecord";
	CONTINUE.
g350_DebitPartyInf_grp2_end.
	EXIT.

g350_DebitPartyInf_grp4.
	%XML_compose "Fee" ent_debit_set.dbt_fee_code "/Fee";
	%XML_compose
	    "SecSubAccount" ent_debit_set.dbt_sec_sub_acct "/SecSubAccount";
	CONTINUE.
g350_DebitPartyInf_grp4_end.
	EXIT.

g375_CreditPartyInf.
*
* Compose CreditPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*
	%XML_compose "CreditPartyInf";

	IF success_is IN CPI_API_wf THEN
	    PERFORM h700_AccountPartyInf THRU h700_AccountPartyInf_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"CreditLocation" ent_credit_set.cdt_location "/CreditLocation";
	END-IF.

	%XML_compose "/CreditPartyInf";
	CONTINUE. 

g375_CreditPartyInf_end.
	EXIT.

g400_IntermediaryBankInf_K.
*
* Compose IntermediaryBankInf IBK data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	  BasicPartyInf  IBK_BPI_wf
*
	%XML_compose "IntermediaryBankInf" ["Type" "IBK"];

	IF success_is IN IBK_BPI_wf THEN
	    PERFORM h800_BasicPartyInf THRU h800_BasicPartyInf_end
	END-IF.

	%XML_compose "SecondWireFlag" ent_credit_set.ibk.ibk_secwir
	    "/SecondWireFlag";

	%XML_compose "/IntermediaryBankInf";
	CONTINUE.

g400_IntermediaryBankInf_K_end.
	EXIT.

g425_IntermediaryBankInf_1.
*
* Compose IntermediaryBankInf IB1 data
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IB1  IB1_wf
*	  BasicPartyInf  IB1_BPI_wf
* 
	%XML_compose "IntermediaryBankInf" ["Type" "IB1"];

	IF success_is IN IB1_BPI_wf THEN
	    PERFORM h850_BasicPartyInf THRU h850_BasicPartyInf_end
	END-IF.

	%XML_compose "SecondWireFlag" ent_credit_set.ib1.ib1_secwir
	    "/SecondWireFlag";

	%XML_compose "/IntermediaryBankInf";
	CONTINUE. 

g425_IntermediaryBankInf_1_end.
	EXIT.

g450_BeneficiaryBankInf.
*
* Compose BeneficiaryBankInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryBankInf  BBI_wf
*         BasicPartyInf  BBI_BPI_wf
*
	%XML_compose "BeneficiaryBankInf";

	IF success_is IN BBI_BPI_wf THEN
	    PERFORM h900_BasicPartyInf THRU h900_BasicPartyInf_end
	END-IF.

	%XML_compose "BeneficiaryIsBank" ent_credit_set.bbk.bbk_secwir
	    "/BeneficiaryIsBank";

	%XML_compose "/BeneficiaryBankInf";
	CONTINUE.

g450_BeneficiaryBankInf_end.
	EXIT.

g475_BeneficiaryPartyInf.
*
* Compose BeneficiaryPartyInf info.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryPartyInf  BPI_wf
*	  BasicPartyInf  BPI_BPI_wf
*
	%XML_compose "BeneficiaryPartyInf";

	IF success_is IN BPI_BPI_wf THEN
	    PERFORM h950_BasicPartyInf THRU h950_BasicPartyInf_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    PERFORM g475_BeneficiaryParty_grp1 THRU
		g475_BeneficiaryParty_grp1_end
	END-IF.

	%XML_compose "BankFlag" ent_credit_set.bnp_bnk_flg "/BankFlag";
	%XML_compose "MailingCountry" ent_credit_set.bnp_mailing_country
	    "/MailingCountry";

	%XML_compose "/BeneficiaryPartyInf";
	CONTINUE. 

g475_BeneficiaryPartyInf_end.
	EXIT.

g475_BeneficiaryParty_grp1.

	%XML_compose "ChargeFlag" ent_credit_set.bnp.charge_flg "/ChargeFlag";
	CONTINUE.

g475_BeneficiaryParty_grp1_end.
	EXIT.

g500_get_prule_with_exec.
*
* Get the next matching prule that has exec param data.
*
* Due to the requirement of 'getting next' from the lowest level of a multi
* input source level structure - a little state machine was created. 
*
* Input:  next_state_wf - what to do next
* Output:  prule_found_wf - success_is or failure_is.
*	   next_state_wf - where to start next time
*
	PERFORM UNTIL success_is IN we_exit_wf
	    EVALUATE next_state_wf

		WHEN %Factor( next_state_wf done_is )
		    %beg prule_found_wf failure_is; %end
		    GO TO g500_get_prule_with_exec_end

		WHEN %Factor( next_state_wf get_config_is )
		    PERFORM h650_get_from_config THRU h650_get_from_config_end

		WHEN %Factor( next_state_wf get_next_prule_is )
		    PERFORM h750_get_prule THRU h750_get_prule_end

		WHEN %Factor( next_state_wf get_next_exec_is )
		    PERFORM h075_get_executable THRU h075_get_executable_end

		WHEN %Factor( next_state_wf get_next_all_exec_is )
		    PERFORM h125_get_all_executable THRU
			h125_get_all_executable_end

		WHEN %Factor( next_state_wf got_one_is )
		    %beg
		    prule_found_wf success_is; 
%^ Set up to get next exec param upon next perform of g500_get_prule_with_exec
		    next_state_wf get_next_exec_is;
		    %end
		    GO TO g500_get_prule_with_exec_end

		WHEN %Factor( next_state_wf got_one_all_is )
		    %beg
		    prule_found_wf success_is;
%^ We are completely done with the current prule here so setup for next call of
%^ g500_get_prule_with_exec
		    next_state_wf get_next_prule_is;
		    %end
		    GO TO g500_get_prule_with_exec_end

	    END-EVALUATE

	END-PERFORM.

g500_get_prule_with_exec_end.
	EXIT.

g600_compose_all_execs.
*
*  compose_seq_ws contains the prule data that we need to compose.
*  Feed it to h175_compose_prule.

	%beg First: compose_seq_ws; %end.

	PERFORM WITH TEST AFTER UNTIL
	  failure_is IN compose_seq_ws_status
	  OR failure_is IN parse_ws_status 

	    %beg
	    parse_ws ^In( compose_seq_ws.txt ),
		    pr_msglevel_ws, "|",
		    tbl_prule_ws, "|",
		    Pr_subtype_ws, "|",
		    prule_param_ws, "|",
		    Prule_param_value_ws, "|";
	    %end
	    IF success_is IN compose_seq_ws_status
	     AND success_is IN parse_ws_status THEN

		IF prule_param_ws = prev_prule_param_ws THEN
* We have multiple values for this executable, so don't tell the compose
* routine to terminate the current <ExecutionParm>.
* The 1st call setting does not matter - gets set to failure
		    CONTINUE
		ELSE
		    %beg
		    prev_prule_param_ws = prule_param_ws;
%^ Tell the compose routine we got a new executable
%^ The 1st call setting does not matter - gets set to failure
		    new_executable_wf success_is; 
		    %end
		END-IF

		PERFORM h175_compose_prule THRU h175_compose_prule_end
	    END-IF

	    %beg Next: compose_seq_ws; %end

	END-PERFORM.

g600_compose_all_execs_end.
	EXIT.

g700_DstHistory.
*
* Check if the current fi_dst_set is composable.
* 
* Output:  rs_wf 
*		success_is - fi_dst_set is ready to compose 
*		failure_is - fi_dst_set is not eligible for composing
*
	IF success_is IN omit_4_stp_wf
	OR (
	 dst_ordinal OF fi_dst_set = 0
	 AND dst_adr_bnk_id OF fi_dst_set = SPACE
	 AND dst_typ OF fi_dst_set = SPACE
	 AND dst_name1 OF fi_dst_set = SPACE
	 AND dst_cab2 OF fi_dst_set = SPACE
	 AND dst_cab3 OF fi_dst_set = SPACE
	 AND dst_cab4 OF fi_dst_set = SPACE
	 AND dst_country OF fi_dst_set = SPACE) THEN 
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

	IF grp_1_ws = 0 
	 AND dst_route_id OF fi_dst_set = SPACE THEN
* Try the next destination set
	    %beg rs_wf failure_is; %end
	    IF success_is IN fi_fi_dst_seq_status THEN
		%beg
		Break: fi_dst_set;
		fi_fi_dst_seq Conn: fi_dst_set;
		%end
	    END-IF
	ELSE
	    %beg rs_wf success_is; %end
	END-IF.

g700_DstHistory_end.
	EXIT.

g800_set_text_seq.
*
* Set fi_text_seq and fi_fi_text_seq based on current history position
*
* Output:
*	Rs_wf -
*	    success_is = fi_text_seq is set
*	    failure_is = fi_text_seq is not set
*
	%beg Break: fi_text_seq; %end.

	IF failure_is IN wrk_msg_history_status THEN
* Just use what caller passed
	    GO TO g800_set_text_seq_cleanup
	END-IF. 
	    
	EVALUATE TRUE
	    WHEN idname OF qname OF wrk_msg_history = "*CDT"
		%beg 
		ent_credit_set(NOTRAP, .pay_seq CONN: fi_text_seq);
		tmp_bool = ent_credit_set status;
		ent_credit_set etrap;
		%end
		IF failure_is IN tmp_bool THEN
* Just use the default
		    GO TO g800_set_text_seq_cleanup
		END-IF

	    WHEN idname OF qname OF wrk_msg_history = "*CDT_SECWIR"
		%beg
		ent_credit_set( Notrap, .secwir_seq CONN: fi_text_seq);
		tmp_bool = ent_credit_set status;
		ent_credit_set etrap;
		%end
		IF failure_is IN tmp_bool THEN
* Just use the default
		    GO TO g800_set_text_seq_cleanup
		END-IF

	    WHEN idname OF qname OF wrk_msg_history = "*CDT_CNF"
	    WHEN idname OF qname OF wrk_msg_history = "*DBT_CNF"
	    WHEN idname OF qname OF wrk_msg_history = "*DLV_CNF"
	    WHEN idname OF qname OF wrk_msg_history = "*SND_CNF"
		%beg
		fi_dst_set (NOMOD, .formatted_text_seq CONN: fi_text_seq);
		tmp_bool = fi_dst_set status;
		fi_dst_set etrap;
		%end
		IF failure_is IN tmp_bool THEN
* Just use the default
		    GO TO g800_set_text_seq_cleanup
		END-IF

	    WHEN OTHER
* Use the default (caller passed ent_text_seq)
		GO TO g800_set_text_seq_cleanup

	END-EVALUATE.

* Got a new one
	%beg First: fi_text_seq; %end.
	IF success_is IN fi_text_seq_status THEN
	    %beg rs_wf success_is; %end
	    GO TO g800_set_text_seq_end
	END-IF.

g800_set_text_seq_cleanup.
	IF success_is IN text_ardy_in_seq_wf THEN
* Don't use passed ent_text_seq more than once
	    %beg rs_wf failure_is; %end
	ELSE
	    %beg
	    text_ardy_in_seq_wf success_is;
	    Break: fi_text_seq; 
	    ent_text_seq Equate: fi_text_seq;
%^ Use it
	    First: fi_text_seq;
	    %end
	    IF success_is IN fi_text_seq_status THEN
	        %beg rs_wf success_is; %end
	    ELSE
		%beg rs_wf failure_is; %end
	    END-IF
	END-IF.

g800_set_text_seq_end.
	EXIT.

g900_dflt_currency.
*
* 	Default the currency code
*
* Output:  currency_code_ws

	IF bank OF loc_info OF ent_ftr_set = bnk_id OF lcl_bnk_union
	 AND bnk_id OF lcl_bnk_union NOT = SPACE THEN
	    %beg currency_code_ws = lcl_bnk_union.base_currency_id; %end
	    GO TO g900_dflt_currency_end
	END-IF.

	%Beg
	BREAK: lcl_bnk_union;
	Bnk_index KEY=Ent_ftr_Set.Loc_info.Bank ^SEARCH CONN: lcl_bnk_union;
	%End

	IF failure_is IN bnk_index_status THEN
	    %beg First: bnk_index; %end
	    If success_is IN bnk_index_status THEN
		%beg bnk_index Conn: lcl_bnk_union; %end
	    ELSE
		%beg currency_code_ws = null; %end
		GO TO g900_dflt_currency_end 
	    END-IF
	END-IF.

	%beg currency_code_ws = lcl_bnk_union.base_currency_id; %end.

G900_dflt_currency_end.
	EXIT.

h000_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    AcctIDInf  DPI_API_AIDI_wf

	IF dbt_ovr OF dbt_typ OF ent_debit_set = SPACE
	 AND idbank OF dbt_account OF ent_debit_set = SPACE
* ignore dbt_idtype without a dbt_id
	 AND dbt_id OF dbt_typ OF ent_debit_set = SPACE
	 AND dbt_bei_flag OF ent_debit_set = SPACE THEN
	    %beg DPI_API_AIDI_wf failure_is; %end
	ELSE
	    %beg DPI_API_AIDI_wf success_is; %end
	END-IF. 

h000_AcctIDInf_end.
	EXIT.

h050_SecondaryID.
*
* Determine if there is any SecondaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*           SecondaryID  DPI_API_SID_wf
*	      AcctIDInf  DPI_API_SID_AIDI_wf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg DPI_API_SID_wf failure_is; %end
	    GO TO h050_SecondaryID_end
	END-IF.

	PERFORM i000_AcctIDInf THRU i000_AcctIDInf_end.

	IF failure-is IN DPI_API_SID_AIDI_wf
	 AND null_flg IN sec_flg OF dbt_acc_2 OF ent_debit_set THEN
	    %beg DPI_API_SID_wf failure_is; %end
	ELSE
	    %beg DPI_API_SID_wf success_is; %end
	END-IF.

h050_SecondaryID_end.
	EXIT.

h075_get_executable.
*
* Get the (next) executable for the current prule.
*
* Output: next_state_wf
*		got_one_is - found an eligible prule executable
*		get_next_prule_is - no executable found, time to try the
*			next prule
*

	PERFORM until success_is IN we_exit_wf

  	    CALL "PRULE_MSG_READEX_PARM" USING
		BY REFERENCE Prule_param_ws
		BY REFERENCE Prule_param_ws_length
		BY REFERENCE Prule_param_type_ws
		BY REFERENCE Prule_param_remain_ws
		BY REFERENCE Prule_param_value_ws
		BY REFERENCE Prule_param_value_ws_length
		RETURNING prule_found_wf

	    IF success_is IN prule_found_wf THEN
		IF tbl_id_ws = "*"
		 OR tbl_id_ws = prule_param_ws THEN
* got one
* todo temprg remove: 
		    DISPLAY "Parm = ",
			prule_param_ws(1: prule_param_ws_length),
			" " prule_param_remain_ws
		    %beg next_state_wf got_one_is; %end
		    IF prule_param_ws = prev_prule_param_ws THEN
* We have multiple values for this executable, so don't tell the compose
* routine to terminate the current <ExecutionParm>
			CONTINUE
		    ELSE 
			%beg
			prev_prule_param_ws = prule_param_ws; 
%^ Tell the compose routine we got a new executable
			new_executable_wf success_is;			
			%end
		    END-IF
		    GO TO h075_get_executable_end
		ELSE
* try the next executable
		    CONTINUE
	    ELSE
* no more executables
		%beg
		next_state_wf get_next_prule_is;
%^ avoid false matches on next prule
		prev_prule_param_ws = " "; 
		%end;
		GO TO h075_get_executable_end
	    END-IF

	END-PERFORM.

h075_get_executable_end.
	EXIT.

h100_TertiaryID.
*
* Determine if there is any TertiaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    TertiaryID  DPI_API_TID_wf
*	      AcctIDInf  DPI_API_TID_AIDI_wf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg DPI_API_TID_wf failure_is; %end
	    GO TO h100_TertiaryID_end
	END-IF.

	PERFORM i050_AcctIDInf THRU i050_AcctIDInf_end.

	IF failure_is IN DPI_API_TID_AIDI_wf
	 AND null_flg IN ter_flg OF dbt_acc_3 OF ent_debit_set THEN
	    %beg DPI_API_TID_wf failure_is; %end
	ELSE
	    %beg DPI_API_TID_wf success_is; %end
	END-IF.

h100_TertiaryID_end.
	EXIT.

h125_get_all_executable.
*
* Perform the (ALL) option requirement.
* We are supposed to select all the PRMs in the prule that contains a given
* PRM.
* Since we don't know when we read the first PRM if we can select or not, we
* put the potential compose data in a temp seq.  Then after we determine if
* this is selectable and complete storing all the prule PRM data in the seq, we
* either use the seq later to compose, or just ignore the temp seq and move on
* to check the next prule. 
*
* Output: next_state_wf
*		got_one_all_is - found an eligible prule, all compose data is
*		    tucked away in compose_seq_ws.  Compose routine will use
*		    compose_seq_ws.
*		get_next_prule_is - no executable found, time to try the next
*		    prule. 
*
	%beg
	Break: compose_seq_ws; 
	Alloc_temp: compose_seq_ws(Mod);
	prule_selectable_wf failure_is;
	%end.

	PERFORM UNTIL success_is IN we_exit_wf

  	    CALL "PRULE_MSG_READEX_PARM" USING
		BY REFERENCE Prule_param_ws
		BY REFERENCE Prule_param_ws_length
		BY REFERENCE Prule_param_type_ws
		BY REFERENCE Prule_param_remain_ws
		BY REFERENCE Prule_param_value_ws
		BY REFERENCE Prule_param_value_ws_length
		RETURNING prule_found_wf

	    IF success_is IN prule_found_wf THEN
		%beg
		Compose_ws ^Out(compose_seq_ws.txt), 
		    pr_msglevel_ws, "|",
		    tbl_prule_ws, "|",
		    Pr_subtype_ws, "|",
		    prule_param_ws, "|",
		    Prule_param_value_ws, "|", / ^Alloc_elem;
		%end
		IF tbl_id_ws = "*"
		 OR tbl_id_ws = prule_param_ws THEN
		    %beg prule_selectable_wf success_is; %end
		END-IF
	    ELSE
* Exhausted all the PRMs (executables) for the current prule - so check what
* happened
		IF success_is IN prule_selectable_wf THEN
* Compose routine will use compose_seq_ws
		    %beg next_state_wf got_one_all_is; %end
		ELSE
* compose_seq_ws is scrap
		    %beg next_state_wf get_next_prule_is; %end
		END-IF
		GO TO h125_get_all_executable_end
	    END-IF

	END-PERFORM.

h125_get_all_executable_end.
	EXIT.

h150_ConcentrationAccount.
*
* Determine if there is any ConcentrationAccount data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    ConcentrationAccount  DPI_API_CA_wf
*	      AcctIDInf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg DPI_API_CA_wf failure_is; %end
	    GO TO h150_ConcentrationAccount_end
	END-IF.

	IF idbank OF dbt_concen_acc OF ent_debit_set = SPACE
* ignore idtype without an idkey
	 AND idkey OF dbt_concen_acc OF ent_debit_set = SPACE THEN
	    %beg DPI_API_CA_wf failure_is; %end
	ELSE
	    %beg DPI_API_CA_wf success_is; %end
	END-IF. 

h150_ConcentrationAccount_end.
	EXIT.

h175_compose_prule.
*
* Do the prule composing
*

* We are guaranteed to have a ParmValue item to compose upon entry positioned
* and ready to compose.  We have set a couple booleans to indicate what we
* need to compose.   Its analogous to 'control breaks' in a report.

	IF success_is IN new_rule_wf THEN
	    %beg
	    new_rule_wf failure_is;
	    new_executable_wf failure_is; 
	    %end
	    IF success_is IN first_prule_wf THEN
%^ Don't close off ExecutionParm tag - not one in progress
		%beg first_prule_wf failure_is; %end
	    ELSE
		%XML_compose "/ExecutionParm";
		%XML_compose "/ProcessingRule";
	    END-IF

	    %XML_compose "ProcessingRule";
* Compose <Level>
	    PERFORM h175b_compose_prule THRU h175b_compose_prule_end
	    %XML_compose "Type" tbl_prule_ws "/Type";
	    %XML_compose "SubType" Pr_subtype_ws "/SubType";
	    %XML_compose "ExecutionParm";
	    %XML_compose "ParmID" prule_param_ws "/ParmID";
	END-IF.

	IF success_is IN new_executable_wf THEN
	    %beg new_executable_wf failure_is; %end
	    %XML_compose "/ExecutionParm";
	    %XML_compose "ExecutionParm";
	    %XML_compose "ParmID" prule_param_ws "/ParmID";
	END-IF.

* And we always need to compose the lowest level ParmValues
	%XML_compose "ParmValues" Prule_param_value_ws "/ParmValues";
	CONTINUE.

h175_compose_prule_end.
	EXIT.

h175b_compose_prule.
*
* Compose the <Level> exceptions values (and non-exceptions value).
* This results in the formatting of the 'original' oneof values STP ACE expects

	EVALUATE pr_msglevel_ws
	  WHEN %Factor( pr_msglevel_ws sbk_msg_ml )
	  WHEN %Factor( pr_msglevel_ws sbk_db_ml )
	    %beg level_ws = "sender"; %end

	  WHEN %Factor( pr_msglevel_ws dbt_msg_ml )
	  WHEN %Factor( pr_msglevel_ws dbt_db_ml )
	    %beg level_ws = "debit_party"; %end

	  WHEN %Factor( pr_msglevel_ws cdt_msg_ml )
	  WHEN %Factor( pr_msglevel_ws cdt_db_ml )
	    %beg level_ws = "credit_party"; %end

	  WHEN OTHER
	    %XML_compose "Level" pr_msglevel_ws "/Level";
	    GO TO h175b_compose_prule_end

	END-EVALUATE.

	%XML_compose "Level" level_ws "/Level";
	CONTINUE.

h175b_compose_prule_end.
	EXIT.

h200_AccountPartyInf.
*
* Determine if there is any CreditPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           AcctIDInf  CPI_API_AIDI_wf
*           SecondaryID  CPI_API_SID_wf
*	    TertiaryID  CPI_API_TID_wf
*	    ConcentrationAccount  CPI_API_CA_wf

	PERFORM i100_AcctIDInf THRU i100_AcctIDInf_end.
	PERFORM i150_SecondaryID THRU i150_SecondaryID_end.
	PERFORM i200_TertiaryID THRU i200_TertiaryID_end. 
	PERFORM i250_ConcentrationAccount THRU i250_ConcentrationAccount_end.

* Handle the xml formatting exclusion requirements of the caller
	IF success_is IN omit_4_isi2_wf
	OR (
	 cdt_acc_parent_code OF ent_credit_set = SPACE
	 AND cdt_acc_prod_codes OF ent_credit_set = SPACE) THEN
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

	IF success_is IN omit_4_isi2_wf
	OR (
	 cdt_department OF ent_credit_set = SPACE
	 AND cdt_sys_of_rec OF ent_credit_set = SPACE) THEN
	    %beg grp_2_ws = <0>; %end
	ELSE
	    %beg grp_2_ws = <1>; %end
	END-IF.

	IF ( cdt_adr_bnk_id OF ent_credit_set = SPACE
	     OR success_is IN omit_4_stp_wf )
	 AND failure_is IN CPI_API_AIDI_wf
	 AND grp_1_ws = 0
	 AND failure_is IN CPI_API_SID_wf
	 AND failure_is IN CPI_API_TID_wf
	 AND cdt_name1 OF ent_credit_set = SPACE
	 AND cdt_name2 OF ent_credit_set = SPACE
	 AND cdt_name3 OF ent_credit_set = SPACE
	 AND cdt_name4 OF ent_credit_set = SPACE
	 AND cdt_adr_class OF ent_credit_set = SPACE
	 AND cdt_adr_city OF ent_credit_set = SPACE
	 AND cdt_adr_country OF ent_credit_set = SPACE 
	 AND cdt_state OF ent_credit_set = SPACE
	 AND cdt_postal_code OF ent_credit_set = SPACE
	 AND cdt_res_country OF ent_credit_set = SPACE
	 AND (success_is IN omit_4_isi2_wf
	      OR cdt_acc_class OF ent_credit_set = SPACE)
	 AND cdt_acc_city OF ent_credit_set = SPACE
	 AND cdt_acc_country OF ent_credit_set = SPACE
	 AND cdt_ref_num OF ent_credit_set = SPACE
	 AND cdt_adv_inst1 OF ent_credit_set = SPACE
	 AND cdt_adv_inst2 OF ent_credit_set = SPACE
	 AND cdt_adv_inst3 OF ent_credit_set = SPACE
	 AND cdt_adv_typ OF ent_credit_set = SPACE
	 AND grp_2_ws = 0
	 AND failure_is IN CPI_API_CA_wf
* ignore idtype without a idkey
	 AND idkey OF cdt_wir_key OF ent_credit_set = SPACE THEN 
	    %beg CPI_API_wf failure_is; %end
	ELSE
	    %beg CPI_API_wf success_is; %end
	END-IF.

h200_AccountPartyInf_end.
	EXIT.

h250_BasicPartyInf.
*
* Determine if there is any BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	  BasicPartyInf  IBK_BPI_wf

	IF ibk_adr_bnk_id OF ent_credit_set = SPACE
* ignore ibk_idtype without a ibk_id
	 AND ibk_id OF ibk OF ent_credit_set = SPACE
	 AND ibk_bei_flag OF ent_credit_set = SPACE
	 AND ibk_name1 OF ent_credit_set = SPACE
	 AND ibk_name2 OF ent_credit_set = SPACE
	 AND ibk_name3 OF ent_credit_set = SPACE
	 AND ibk_name4 OF ent_credit_set = SPACE
	 AND ibk_state OF ent_credit_set = SPACE
	 AND ibk_postal_code OF ent_credit_set = SPACE
	 AND ibk_res_country OF ent_credit_set = SPACE
	 AND ibk_ref_num OF ent_credit_set = SPACE
	 AND ibk_adv_inst1 OF ent_credit_set = SPACE
	 AND ibk_adv_inst2 OF ent_credit_set = SPACE
	 AND ibk_adv_inst3 OF ent_credit_set = SPACE THEN
	    %beg IBK_BPI_wf failure_is; %end
	ELSE
	    %beg IBK_BPI_wf success_is; %end
	END-IF. 

h250_BasicPartyInf_end.
	EXIT.

h300_BasicPartyInf.
*
* Determine if there is any BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IB1  IB1_wf
*	  BasicPartyInf  IB1_BPI_wf

	IF ib1_adr_bnk_id OF ent_credit_set = SPACE
* ignore ib1_idtype without a ib1_id
	 AND ib1_id OF ib1 OF ent_credit_set = SPACE
	 AND ib1_bei_flag OF ent_credit_set = SPACE
	 AND ib1_name1 OF ent_credit_set = SPACE
	 AND ib1_name2 OF ent_credit_set = SPACE
	 AND ib1_name3 OF ent_credit_set = SPACE
	 AND ib1_name4 OF ent_credit_set = SPACE
	 AND ib1_state OF ent_credit_set = SPACE
	 AND ib1_postal_code OF ent_credit_set = SPACE
	 AND ib1_res_country OF ent_credit_set = SPACE
	 AND ib1_ref_num OF ent_credit_set = SPACE
	 AND ib1_adv_inst1 OF ent_credit_set = SPACE
	 AND ib1_adv_inst2 OF ent_credit_set = SPACE
	 AND ib1_adv_inst3 OF ent_credit_set = SPACE THEN
	    %beg IB1_BPI_wf failure_is; %end
	ELSE
	    %beg IB1_BPI_wf success_is; %end
	END-IF. 

h300_BasicPartyInf_end.
	EXIT.

h350_BasicPartyInf.
*
* Determine if there is any BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryBankInf  BBI_wf
*         BasicPartyInf  BBI_BPI_wf

	IF bbk_adr_bnk_id OF ent_credit_set = SPACE
* ignore bbk_idtype without a bbk_id
	 AND bbk_id OF bbk OF ent_credit_set = SPACE
	 AND bbk_bei_flag OF ent_credit_set = SPACE
	 AND bbk_name1 OF ent_credit_set = SPACE
	 AND bbk_name2 OF ent_credit_set = SPACE
	 AND bbk_name3 OF ent_credit_set = SPACE
	 AND bbk_name4 OF ent_credit_set = SPACE
	 AND bbk_state OF ent_credit_set = SPACE
	 AND bbk_postal_code OF ent_credit_set = SPACE
	 AND bbk_res_country OF ent_credit_set = SPACE
	 AND bbk_ref_num OF ent_credit_set = SPACE
	 AND bbk_adv_inst1 OF ent_credit_set = SPACE
	 AND bbk_adv_inst2 OF ent_credit_set = SPACE
	 AND bbk_adv_inst3 OF ent_credit_set = SPACE THEN
	    %beg BBI_BPI_wf failure_is; %end
	ELSE
	    %beg BBI_BPI_wf success_is; %end
	END-IF. 

h350_BasicPartyInf_end.
	EXIT.

h400_BasicPartyInf.
*
* Determine if there is any BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryPartyInf  BPI_wf
*	  BasicPartyInf  BPI_BPI_wf

* ignore bnp_idtype without a bnp_id
	IF bnp_id OF bnp OF ent_credit_set = SPACE
	 AND bnp_bei_flag OF ent_credit_set = SPACE
	 AND bnp_name1 OF ent_credit_set = SPACE
	 AND bnp_name2 OF ent_credit_set = SPACE
	 AND bnp_name3 OF ent_credit_set = SPACE
	 AND bnp_name4 OF ent_credit_set = SPACE
	 AND bnp_state OF ent_credit_set = SPACE
	 AND bnp_postal_code OF ent_credit_set = SPACE
	 AND bnp_res_country OF ent_credit_set = SPACE
	 AND bnp_ref_num OF ent_credit_set = SPACE
	 AND bnp_adv_inst1 OF ent_credit_set = SPACE
	 AND bnp_adv_inst2 OF ent_credit_set = SPACE
	 AND bnp_adv_inst3 OF ent_credit_set = SPACE THEN
	    %beg BPI_BPI_wf failure_is; %end
	ELSE
	    %beg BPI_BPI_wf success_is; %end
	END-IF. 

h400_BasicPartyInf_end.
	EXIT.

h450_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    AcctIDInf  DPI_API_AIDI_wf
*
	%XML_compose "AcctIDInf";

	%XML_compose "Override" ent_debit_set.dbt_typ.dbt_ovr "/Override";
	%XML_compose "IDBank" ent_debit_set.dbt_account.idbank "/IDBank";

	%beg
	tag_id_ws = ent_debit_set.dbt_typ.dbt_id;
	tag_idtype_ws = ent_debit_set.dbt_typ.dbt_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_debit_set.dbt_bei_flag "/BEIFlag";

	%XML_compose "/AcctIDInf";
	CONTINUE.

h450_AcctIDInf_end.
	EXIT.

h500_SecondaryID.
*
* Compose SecondaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*           SecondaryID  DPI_API_SID_wf
*	      AcctIDInf  DPI_API_SID_AIDI_wf
*
	%XML_compose "SecondaryID";

	IF success_is IN DPI_API_SID_AIDI_wf THEN
	    PERFORM i300_AcctIDInf THRU i300_AcctIDInf_end
	END-IF.

	IF null_flg IN sec_flg OF dbt_acc_2 OF ent_debit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Flag" ent_debit_set.dbt_acc_2.sec_flg "/Flag";
	END-IF. 

	%XML_compose "/SecondaryID";
	CONTINUE.

h500_SecondaryID_end.
	EXIT.

h550_TertiaryID.
*
* Compose TertiaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    TertiaryID  DPI_API_TID_wf
*	      AcctIDInf  DPI_API_TID_AIDI_wf
*
	%XML_compose "TertiaryID"; 

	IF success_is IN DPI_API_TID_AIDI_wf THEN
	    PERFORM i350_AcctIDInf THRU i350_AcctIDInf_end
	END-IF.

	IF null_flg IN ter_flg OF dbt_acc_3 OF ent_debit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Flag" ent_debit_set.dbt_acc_3.ter_flg "/Flag";
	END-IF. 

	%XML_compose "/TertiaryID";
	CONTINUE.

h550_TertiaryID_end.
	EXIT.

h600_ConcentrationAccount.
*
* Compose ConcentrationAccount data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    ConcentrationAccount  DPI_API_CA_wf
*	      AcctIDInf
*
	%XML_compose "ConcentrationAccount";
	%XML_compose "AcctIDInf";

	%XML_compose "IDBank" ent_debit_set.dbt_concen_acc.idbank "/IDBank";

	%beg
	tag_id_ws =  ent_debit_set.dbt_concen_acc.idkey;
	tag_idtype_ws = ent_debit_set.dbt_concen_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	%XML_compose "/ConcentrationAccount";
	CONTINUE.

h600_ConcentrationAccount_end.
	EXIT.

h650_get_from_config.
*
* Get the next prule we need to format/compose from the config table.
*
* Output:  next_state_wf
*		get_next_prule_is - got the next config entry.  time to get the
*			the prule
*		done_is - config table has been exhausted - all done

	PERFORM until success_is IN we_exit_wf

	    ADD +1 TO cfg_seq_ordinal_ws

	    Call "CFG_GET_ITEM" using
		by reference Idname of Cfg_union_key_ws
		by reference Idprod of Cfg_union_key_ws
		by reference Idbank of Cfg_union_key_ws
		by reference Idloc  of Cfg_union_key_ws
		by reference Idcust of Cfg_union_key_ws
		by reference Cfg_item_key_ws
		by reference Cfg_seq_ordinal_ws
		by reference Cfg_item_type_ws
		by reference Cfg_item_data_ws
		by reference Cfg_item_data_ws_length
		by reference Cfg_error_msg_ws
		by reference Cfg_error_msg_ws_length
		returning prule_found_wf
	    IF failure_is IN prule_found_wf THEN
		%beg next_state_wf done_is; %end
		GO TO h650_get_from_config_end
	    END-IF

* Valid formats are:
*	<caller>|<prule>|<id>
*	<caller>|<prule>|<id>(ALL)
	    %beg
	    parse_ws ^In(cfg_item_data_ws),
		tbl_caller_ws, "|",
		tbl_prule_ws, "|",
		option_ws(
		    ^Oneof(
			(|tbl_id_ws, "(ALL)",/),
			(|tbl_id_ws,/)));
	    %end
		    
	    IF failure_is IN parse_ws_status THEN
		%beg prule_found_wf failure_is; %end
		CALL "NEX_CREATE_AND_BROADCAST_MSG" USING
		    BY CONTENT Z"XML$_BAD_CONFIG",
		    BY VALUE -1,
		    %ace_msg_arg_list(cfg_item_data_ws);
* just skip the invalid row, keep going
	    ELSE
* row is valid - check if the caller matches
		IF tbl_caller_ws =  app_id_ls THEN
* Got one
* Finally check is the (ALL) option was specified
		    IF option_ws = 0 THEN 		
			%beg all_execs_option_wf success_is; %end
		    ELSE
			%beg all_execs_option_wf failure_is; %end
		    END-IF

		    %beg 
		    pr_ordinal_ws = <0>;
		    next_state_wf get_next_prule_is;
		    %end
		    GO TO h650_get_from_config_end
		END-IF
	    END-IF

	END-PERFORM.		
		
h650_get_from_config_end.
	EXIT.

h700_AccountPartyInf.
*
* Compose AccountPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           AcctIDInf  CPI_API_AIDI_wf
*           SecondaryID  CPI_API_SID_wf
*	    TertiaryID  CPI_API_TID_wf
*	    ConcentrationAccount  CPI_API_CA_wf
*
	%XML_compose "AccountPartyInf";

	IF success_is IN omit_4_stp_wf THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"AdrBankID" ent_credit_set.cdt_adr_bnk_id "/AdrBankID";
	END-IF.

	IF success_is IN CPI_API_AIDI_wf THEN
	    PERFORM i400_AcctIDInf THRU i400_AcctIDInf_end
	END-IF.

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    PERFORM h700_AccountPartyInf_grp1 THRU
		h700_AccountPartyInf_grp1_end
	END-IF.

	IF success_is IN CPI_API_SID_wf THEN
	    PERFORM i450_SecondaryID THRU i450_SecondaryID_end
	END-IF.

	IF success_is IN CPI_API_TID_wf THEN
	    PERFORM i500_TertiaryID THRU i500_TertiaryID_end
	END-IF.

	%XML_compose
	    "AddressInf" ["Seq" "1"] ent_credit_set.cdt_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"] ent_credit_set.cdt_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"] ent_credit_set.cdt_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"] ent_credit_set.cdt_name4 "/AddressInf";
	%XML_compose
		"AddressClass" ent_credit_set.cdt_adr_class "/AddressClass";
	%XML_compose "AddressCity" ent_credit_set.cdt_adr_city "/AddressCity";
	%XML_compose
	    "AddressCountry" ent_credit_set.cdt_adr_country "/AddressCountry";
	%XML_compose "State" ent_credit_set.cdt_state "/State";
	%XML_compose "PostalCode" ent_credit_set.cdt_postal_code "/PostalCode";
	%XML_compose "ResidenceCountry" ent_credit_set.cdt_res_country
	    "/ResidenceCountry";

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    %XML_compose
		"AccountClass" ent_credit_set.cdt_acc_class "/AccountClass";
	END-IF. 

	%XML_compose "AccountCity" ent_credit_set.cdt_acc_city "/AccountCity";
	%XML_compose
	    "AccountCountry" ent_credit_set.cdt_acc_country "/AccountCountry";
	%XML_compose "Reference" ent_credit_set.cdt_ref_num "/Reference";
	%XML_compose "AdviceInstructions" ["Type" "INST1"]
	    ent_credit_set.cdt_adv_inst1 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST2"]
	    ent_credit_set.cdt_adv_inst2 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST3"]
	    ent_credit_set.cdt_adv_inst3 "/AdviceInstructions";
	%XML_compose "AdviceType" ent_credit_set.cdt_adv_typ "/AdviceType";

	IF success_is IN omit_4_isi2_wf THEN
	    CONTINUE
	ELSE
	    PERFORM
		h700_AccountPartyInf_grp2 THRU h700_AccountPartyInf_grp2_end
	END-IF.

	IF success_is IN CPI_API_CA_wf THEN
	    PERFORM i550_ConcentrationAccount THRU
		i550_ConcentrationAccount_end
	END-IF.

	IF idkey OF cdt_wir_key OF ent_credit_set = SPACE THEN
	    CONTINUE
	ELSE
	    %XML_compose "WireKey";
	    %XML_compose "AcctIDInf"; 

	    %beg
	    tag_id_ws = ent_credit_set.cdt_wir_key.idkey;
	    tag_idtype_ws = ent_credit_set.cdt_wir_key.idtype;
	    tag_ws = "ID";
	    %end
	    PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end 

	    %XML_compose "/AcctIDInf"; 
	    %XML_compose "/WireKey";
	END-IF.

	%XML_compose "/AccountPartyInf";
	CONTINUE. 

h700_AccountPartyInf_end.
	EXIT.

h700_AccountPartyInf_grp1.
	%XML_compose
	    "ParentCode" ent_credit_set.cdt_acc_parent_code "/ParentCode";
	%XML_compose
	    "ProductCode" ent_credit_set.cdt_acc_prod_codes "/ProductCode";
	CONTINUE.
h700_AccountPartyInf_grp1_end.
	EXIT.

h700_AccountPartyInf_grp2.

	%XML_compose "Department" ent_credit_set.cdt_department "/Department";
	%XML_compose
	    "SystemofRecord" ent_credit_set.cdt_sys_of_rec "/SystemofRecord";
	CONTINUE.

h700_AccountPartyInf_grp2_end.
	EXIT.

h750_get_prule.
*
* Get the next prule - the config table is driving us.
*
* Output:  next_state_wf
*		get_config_is - could not find a prule
*		get_next_exec_is - got a prule, time to get the executables
*

	CALL "PRULE_MSG_RULE_MATCH" USING
	    BY REFERENCE tbl_prule_ws
	    BY REFERENCE tbl_prule_ws_length
	    BY REFERENCE Pr_ordinal_ws
	    BY REFERENCE pr_msglevel_ws
	    BY REFERENCE pr_source_ws
	    BY REFERENCE Pr_subtype_ws
	    BY REFERENCE Pr_subtype_ws_length
	    BY REFERENCE Pr_memo_ws
	    BY REFERENCE Pr_memo_ws_length
	    RETURNING prule_found_wf.

	IF success_is IN prule_found_wf THEN
	    %beg
	    prule_param_ws = " ";
	    prule_param_remain_ws = <0>;
	    %end
	    IF success_is IN all_execs_option_wf THEN
		%beg next_state_wf get_next_all_exec_is; %end
	    ELSE
		%beg next_state_wf get_next_exec_is; %end
	    END-IF
* Tell the compose routine that we got a new prule
	    %beg new_rule_wf success_is; %end
	ELSE
	    %beg next_state_wf get_config_is; %end
	END-IF.

h750_get_prule_end.
	EXIT.

h800_BasicPartyInf.
*
* Compose BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IBK  IBK_wf
*	  BasicPartyInf  IBK_BPI_wf
*
	%XML_compose "BasicPartyInf";

	%XML_compose "AdrBankID" ent_credit_set.ibk_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_credit_set.ibk.ibk_id;
	tag_idtype_ws = ent_credit_set.ibk.ibk_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_credit_set.ibk_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"]  ent_credit_set.ibk_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"]  ent_credit_set.ibk_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"]  ent_credit_set.ibk_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"]  ent_credit_set.ibk_name4 "/AddressInf";
	%XML_compose "State" ent_credit_set.ibk_state "/State";
	%XML_compose "PostalCode" ent_credit_set.ibk_postal_code "/PostalCode";
	%XML_compose "Country" ent_credit_set.ibk_res_country "/Country";
	%XML_compose "Reference" ent_credit_set.ibk_ref_num "/Reference";
	%XML_compose "AdviceInstructions" ["Type" "INST1"]
	    ent_credit_set.ibk_adv_inst1 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST2"]
	    ent_credit_set.ibk_adv_inst2 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST3"]
	    ent_credit_set.ibk_adv_inst3 "/AdviceInstructions";

	%XML_compose "/BasicPartyInf";
	CONTINUE. 

h800_BasicPartyInf_end.
	EXIT.

h850_BasicPartyInf.
*
* Compose BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	IntermediaryBankInf IB1  IB1_wf
*	  BasicPartyInf  IB1_BPI_wf
*
	%XML_compose "BasicPartyInf"; 

	%XML_compose "AdrBankID" ent_credit_set.ib1_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_credit_set.ib1.ib1_id;
	tag_idtype_ws = ent_credit_set.ib1.ib1_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_credit_set.ib1_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"]  ent_credit_set.ib1_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"]  ent_credit_set.ib1_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"]  ent_credit_set.ib1_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"]  ent_credit_set.ib1_name4 "/AddressInf";
	%XML_compose "State" ent_credit_set.ib1_state "/State";
	%XML_compose "PostalCode" ent_credit_set.ib1_postal_code "/PostalCode";
	%XML_compose "Country" ent_credit_set.ib1_res_country "/Country";
	%XML_compose "Reference" ent_credit_set.ib1_ref_num "/Reference";
	%XML_compose "AdviceInstructions" ["Type" "INST1"]
	    ent_credit_set.ib1_adv_inst1 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST2"]
	    ent_credit_set.ib1_adv_inst2 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST3"]
	    ent_credit_set.ib1_adv_inst3 "/AdviceInstructions";

	%XML_compose "/BasicPartyInf";
	CONTINUE. 

h850_BasicPartyInf_end.
	EXIT.

h900_BasicPartyInf.
*
* Compose BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryBankInf  BBI_wf
*         BasicPartyInf  BBI_BPI_wf
*
	%XML_compose "BasicPartyInf"; 

	%XML_compose "AdrBankID" ent_credit_set.bbk_adr_bnk_id "/AdrBankID";

	%beg
	tag_id_ws = ent_credit_set.bbk.bbk_id;
	tag_idtype_ws = ent_credit_set.bbk.bbk_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_credit_set.bbk_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"]  ent_credit_set.bbk_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"]  ent_credit_set.bbk_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"]  ent_credit_set.bbk_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"]  ent_credit_set.bbk_name4 "/AddressInf";
	%XML_compose "State" ent_credit_set.bbk_state "/State";
	%XML_compose "PostalCode" ent_credit_set.bbk_postal_code "/PostalCode";
	%XML_compose "Country" ent_credit_set.bbk_res_country "/Country";
	%XML_compose "Reference" ent_credit_set.bbk_ref_num "/Reference";
	%XML_compose "AdviceInstructions" ["Type" "INST1"]
	    ent_credit_set.bbk_adv_inst1 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST2"]
	    ent_credit_set.bbk_adv_inst2 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST3"]
	    ent_credit_set.bbk_adv_inst3 "/AdviceInstructions";

	%XML_compose "/BasicPartyInf";
	CONTINUE.

h900_BasicPartyInf_end.
	EXIT.

h950_BasicPartyInf.
*
* Compose BasicPartyInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	BeneficiaryPartyInf  BPI_wf
*	  BasicPartyInf  BPI_BPI_wf
*
	%XML_compose "BasicPartyInf";

	%beg
	tag_id_ws = ent_credit_set.bnp.bnp_id;
	tag_idtype_ws = ent_credit_set.bnp.bnp_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_credit_set.bnp_bei_flag "/BEIFlag";
	%XML_compose
	    "AddressInf" ["Seq" "1"]  ent_credit_set.bnp_name1 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "2"]  ent_credit_set.bnp_name2 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "3"]  ent_credit_set.bnp_name3 "/AddressInf";
	%XML_compose
	    "AddressInf" ["Seq" "4"]  ent_credit_set.bnp_name4 "/AddressInf";
	%XML_compose "State" ent_credit_set.bnp_state "/State";
	%XML_compose "PostalCode" ent_credit_set.bnp_postal_code "/PostalCode";
	%XML_compose "Country" ent_credit_set.bnp_res_country "/Country";
	%XML_compose "Reference" ent_credit_set.bnp_ref_num "/Reference";
	%XML_compose "AdviceInstructions" ["Type" "INST1"]
	    ent_credit_set.bnp_adv_inst1 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST2"]
	    ent_credit_set.bnp_adv_inst2 "/AdviceInstructions";
	%XML_compose "AdviceInstructions" ["Type" "INST3"]
	    ent_credit_set.bnp_adv_inst3 "/AdviceInstructions";

	%XML_compose "/BasicPartyInf";
	CONTINUE. 

h950_BasicPartyInf_end.
	EXIT.

i000_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*           SecondaryID  DPI_API_SID_wf
*	      AcctIDInf  DPI_API_SID_AIDI_wf

	IF idbank OF sec_acc OF dbt_acc_2 OF ent_debit_set = SPACE
* ignore idtype without a idkey
	 AND idkey OF sec_acc OF dbt_acc_2 OF ent_debit_set = SPACE THEN
	    %beg DPI_API_SID_AIDI_wf failure_is; %end
	ELSE
	    %beg DPI_API_SID_AIDI_wf success_is; %end
	END-IF.

i000_AcctIDInf_end.
	EXIT.

i050_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    TertiaryID  DPI_API_TID_wf
*	      AcctIDInf  DPI_API_TID_AIDI_wf

	IF idbank OF ter_acc OF dbt_acc_3 OF ent_debit_set = SPACE
* ignore idtype without an idkey
	 AND idkey OF ter_acc OF dbt_acc_3 OF ent_debit_set = SPACE THEN
	    %beg DPI_API_TID_AIDI_wf failure_is; %end
	ELSE
	    %beg DPI_API_TID_AIDI_wf success_is; %end
	END-IF.

i050_AcctIDInf_end.
	EXIT.

i100_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           AcctIDInf  CPI_API_AIDI_wf

	IF success_is IN omit_4_stp_wf
	OR (
	     cdt_ovr OF cdt_typ OF ent_credit_set = SPACE
         AND idbank OF cdt_account OF ent_credit_set = SPACE) THEN
	    %beg grp_1_ws = <0>; %end
	ELSE
	    %beg grp_1_ws = <1>; %end
	END-IF.

* ignore idtype without a id
	IF grp_1_ws = 0 
	 AND cdt_id OF cdt_typ OF ent_credit_set = SPACE
	 AND cdt_bei_flag OF ent_credit_set = SPACE THEN
	    %beg CPI_API_AIDI_wf failure_is; %end
	ELSE
	    %beg CPI_API_AIDI_wf success_is; %end
	END-IF. 

i100_AcctIDInf_end.
	EXIT.

i150_SecondaryID.
*
* Determine if there is any SecondaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           SecondaryID  CPI_API_SID_wf
*	      AcctIDInf  CPI_API_SID_AIDI_wf

	PERFORM j000_AcctIDInf THRU j000_AcctIDInf_end.

	IF failure_is IN CPI_API_SID_AIDI_wf
	 AND (null_flg IN sec_flg OF cdt_acc_2 OF ent_credit_set
	      OR success_is IN omit_4_isi2_wf) THEN
	    %beg CPI_API_SID_wf failure_is; %end
	ELSE
	    %beg CPI_API_SID_wf success_is; %end
	END-IF.

i150_SecondaryID_end.
	EXIT.

i200_TertiaryID.
*
* Determine if there is any TertiaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    TertiaryID  CPI_API_TID_wf
*	      AcctIDInf  CPI_API_TID_AIDI_wf

	PERFORM j100_AcctIDInf THRU j100_AcctIDInf_end.

	IF failure_is IN CPI_API_TID_AIDI_wf
	 AND (null_flg IN ter_flg OF cdt_acc_3 OF ent_credit_set
	      OR success_is IN omit_4_isi2_wf) THEN
	    %beg CPI_API_TID_wf failure_is; %end	 
	ELSE
	    %beg CPI_API_TID_wf success_is; %end	 
	END-IF.    
	    
i200_TertiaryID_end.
	EXIT.

i250_ConcentrationAccount.
*
* Determine if there is any ConcentrationAccount data.
* 
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    ConcentrationAccount  CPI_API_CA_wf
*	      AcctIDInf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg CPI_API_CA_wf failure_is; %end
	    GO TO i250_ConcentrationAccount_end
	END-IF.

	IF idbank OF cdt_concen_acc OF ent_credit_set = SPACE
* ignore idtype without a idkey
	 AND idkey OF cdt_concen_acc OF ent_credit_set THEN 
	    %beg CPI_API_CA_wf failure_is; %end
	ELSE
	    %beg CPI_API_CA_wf success_is; %end
	END-IF.     
	    
i250_ConcentrationAccount_end.
	EXIT.

i300_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*           SecondaryID  DPI_API_SID_wf
*	      AcctIDInf  DPI_API_SID_AIDI_wf
*
	%XML_compose "AcctIDInf";

	%beg vstr132_ws = ent_debit_set.dbt_acc_2.sec_acc.idbank; %end
	%XML_compose "IDBank" vstr132_ws "/IDBank";

	%beg
	tag_id_ws = ent_debit_set.dbt_acc_2.sec_acc.idkey;
	tag_idtype_ws = ent_debit_set.dbt_acc_2.sec_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	CONTINUE.

i300_AcctIDInf_end.
	EXIT.

i350_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	DebitPartyInf
*	  AccountPartyInf  DPI_API_wf
*	    TertiaryID  DPI_API_TID_wf
*	      AcctIDInf  DPI_API_TID_AIDI_wf
*
	%XML_compose "AcctIDInf";

	%beg vstr132_ws = ent_debit_set.dbt_acc_3.ter_acc.idbank; %end
	%XML_compose "IDBank" vstr132_ws "/IDBank";

	%beg
	tag_id_ws = ent_debit_set.dbt_acc_3.ter_acc.idkey;
	tag_idtype_ws = ent_debit_set.dbt_acc_3.ter_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	CONTINUE.

i350_AcctIDInf_end.
	EXIT.

i400_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           AcctIDInf  CPI_API_AIDI_wf
*
	%XML_compose "AcctIDInf";

	IF success_is IN omit_4_stp_wf THEN
	    CONTINUE
	ELSE
	    PERFORM i400_AcctIDInf_grp1 THRU i400_AcctIDInf_grp1_end
	END-IF.

	%beg
	tag_id_ws = ent_credit_set.cdt_typ.cdt_id;
	tag_idtype_ws = ent_credit_set.cdt_typ.cdt_idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "BEIFlag" ent_credit_set.cdt_bei_flag "/BEIFlag";

	%XML_compose "/AcctIDInf";
	CONTINUE. 

i400_AcctIDInf_end.
	EXIT.

i400_AcctIDInf_grp1.

	%XML_compose "Override" ent_credit_set.cdt_typ.cdt_ovr "/Override";
	%XML_compose "IDBank" ent_credit_set.cdt_account.idbank "/IDBank";

	CONTINUE.

i400_AcctIDInf_grp1_end.
	EXIT.

i450_SecondaryID.
*
* Compose SecondaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           SecondaryID  CPI_API_SID_wf
*	      AcctIDInf  CPI_API_SID_AIDI_wf
*
	%XML_compose "SecondaryID";

	IF success_is IN CPI_API_SID_AIDI_wf THEN
	    PERFORM j200_AcctIDInf THRU j200_AcctIDInf_end
	END-IF.

	IF null_flg IN sec_flg OF cdt_acc_2 OF ent_credit_set THEN
	    CONTINUE
	ELSE
	    %XML_compose "Flag" ent_credit_set.cdt_acc_2.sec_flg "/Flag";
	END-IF.

	%XML_compose "/SecondaryID";
	CONTINUE. 

i450_SecondaryID_end.
	EXIT.

i500_TertiaryID.
*
* Compose TertiaryID data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    TertiaryID  CPI_API_TID_wf
*	      AcctIDInf  CPI_API_TID_AIDI_wf
*
	%XML_compose "TertiaryID";

	IF success_is IN CPI_API_TID_AIDI_wf THEN
	    PERFORM j300_AcctIDInf THRU j300_AcctIDInf_end
	END-IF.

	IF null_flg IN ter_flg OF cdt_acc_3 OF ent_credit_set
	    CONTINUE
	ELSE
	    %XML_compose "Flag" ent_credit_set.cdt_acc_3.ter_flg "/Flag";
	END-IF. 

	%XML_compose "/TertiaryID";
	CONTINUE. 

i500_TertiaryID_end.
	EXIT.

i550_ConcentrationAccount.
*
* Compose ConcentrationAccount data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    ConcentrationAccount  CPI_API_CA_wf
*	      AcctIDInf
*
	%XML_compose "ConcentrationAccount";
	%XML_compose "AcctIDInf";

	%XML_compose "IDBank" ent_credit_set.cdt_concen_acc.idbank "/IDBank";

	%beg
	tag_id_ws = ent_credit_set.cdt_concen_acc.idkey;
	tag_idtype_ws = ent_credit_set.cdt_concen_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	%XML_compose "/ConcentrationAccount";
	CONTINUE. 

i550_ConcentrationAccount_end.
	EXIT.

j000_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           SecondaryID  CPI_API_SID_wf
*	      AcctIDInf  CPI_API_SID_AIDI_wf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg CPI_API_SID_AIDI_wf failure_is;  %end
	    GO TO j000_AcctIDInf_end
	END-IF.

	IF idbank OF sec_acc OF cdt_acc_2 OF ent_credit_set = SPACE
* ignore idtype withiout a idkey
	 AND idkey OF sec_acc OF cdt_acc_2 OF ent_credit_set = SPACE THEN
	    %beg CPI_API_SID_AIDI_wf failure_is;  %end
	ELSE
	    %beg CPI_API_SID_AIDI_wf success_is;  %end
	END-IF.

j000_AcctIDInf_end.
	EXIT.

j100_AcctIDInf.
*
* Determine if there is any AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    TertiaryID  CPI_API_TID_wf
*	      AcctIDInf  CPI_API_TID_AIDI_wf

	IF success_is IN omit_4_isi2_wf THEN
	    %beg CPI_API_TID_AIDI_wf failure_is; %end
	    GO TO j100_AcctIDInf_end
	END-IF.
	    
	IF idbanK OF ter_acc OF cdt_acc_3 OF ent_credit_set = SPACE
* ignore idtype without a idkey
	 AND idkey OF ter_acc OF cdt_acc_3 OF ent_credit_set = SPACE THEN
	    %beg CPI_API_TID_AIDI_wf failure_is; %end
	ELSE
	    %beg CPI_API_TID_AIDI_wf success_is; %end
	END-IF.    
	
j100_AcctIDInf_end.
	EXIT.

j200_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*           SecondaryID  CPI_API_SID_wf
*	      AcctIDInf  CPI_API_SID_AIDI_wf
*
	%XML_compose "AcctIDInf";

	%beg vstr132_ws = ent_credit_set.cdt_acc_2.sec_acc.idbank; %end
	%XML_compose "IDBank" vstr132_ws "/IDBank";

	%beg
	tag_id_ws = ent_credit_set.cdt_acc_2.sec_acc.idkey;
	tag_idtype_ws = ent_credit_set.cdt_acc_2.sec_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	CONTINUE. 

j200_AcctIDInf_end.
	EXIT.

j300_AcctIDInf.
*
* Compose AcctIDInf data.
*
* Message  M_wf
*   BasicPayment  BP_wf
*     PartyInf  PI_wf
*	CreditPartyInf  CPI_wf
*	  AccountPartyInf  CPI_API_wf
*	    TertiaryID  CPI_API_TID_wf
*	      AcctIDInf  CPI_API_TID_AIDI_wf
*
	%XML_compose "AcctIDInf";

	%beg vstr132_ws = ent_credit_set.cdt_acc_3.ter_acc.idbank; %end
	%XML_compose "IDBank" vstr132_ws "/IDBank";

	%beg
	tag_id_ws = ent_credit_set.cdt_acc_3.ter_acc.idkey;
	tag_idtype_ws = ent_credit_set.cdt_acc_3.ter_acc.idtype;
	tag_ws = "ID";
	%end.
	PERFORM k000_tag_id_idtype THRU k000_tag_id_idtype_end.

	%XML_compose "/AcctIDInf";
	CONTINUE. 

j300_AcctIDInf_end.
	EXIT.

k000_tag_id_idtype.
*
* Input:
*	tag_id_ws
*	tag_idtype_ws
*	tag_ws
*
* Compose a
*	<tag_ws Type=tag_idtype_ws>tag_id_ws</tag_ws>
* structure.
* Note when tag_idtype_ws if blank, we compose as
*	<tag_ws>tag_id_ws</tag_ws>

	IF tag_id_ws = SPACE THEN 
	    CONTINUE
	ELSE
* macro does not support variable types nor variable tags so ...
	    CALL "NEX_XML_SEND" USING
		BY VALUE tag_id_ws_length
		BY REFERENCE tag_id_ws
		BY VALUE 400
		BY REFERENCE temp_tagvar_ws_length
		BY REFERENCE temp_tagvar_ws
	    %beg ifml_compose '<' tag_ws; %end
	    IF tag_idtype_ws = SPACE THEN
* ID only without IDTYPE
		CONTINUE
	    ELSE
	        %beg
		ifml_compose ' Type="' tag_idtype_ws '"';
		%end
	    END-IF
	    %beg ifml_compose '>' temp_tagvar_ws '</' tag_ws '>'; %end
	END-IF.

k000_tag_id_idtype_end.
	EXIT.




