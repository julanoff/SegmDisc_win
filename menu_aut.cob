%Module MENU_AUT;

*****************************************************************
*								*
* Copyright 1998 - 2001 by IntraNet, Inc. All rights reserved.	*
*								*
* This Software is confidential and proprietary to IntraNet	*
* and it is protected by U.S. copyright law, other national	*
* copyright laws, and international treaties. The Software may	*
* not be disclosed or reproduced in whole or in part in any	*
* manner to any third party without the express prior written	*
* consent of IntraNet, Inc.					*
*								*
* This Software and its related Documentation are made		*
* available under the terms of the Software License		*
* and may not be used, reproduced or disclosed in any manner	*
* except as expressly authorized by the Software License. All	*
* other uses are strictly prohibited.				*
*								*
* This Software and its related Documentation are proprietary	*
* and confidential material of IntraNet, Inc.			*
*								*
*****************************************************************

*  o  The "AUT" Enable/disable operator function.		*

%^ Handle Authorization Enable/Disable Operator/Terminal Commands.
%^ Assumes operator has proper privilege.

%^ Modification History
%^ 	Aftab Bhaiwala	Jan 09, 2001
%^		Per 68306. Set last login time to current time so that
%^		the operator is not considered inactive.
%^
%^	Joseph Carey	Feb 26, 2001
%^		Per 67692 - Unlock User (also called "Reset" User).
%^		Enhance the AUT screen to provide a new
%^		selection, "R" (= Reset).  
%^		The Reset function is used when an operator's process 
%^		terminates abnormally and the operator cannot log
%^		on again.  The Reset function allows a supervisor to
%^		reset the operator's status so that the operator
%^		can log on again. 
*
* T. Belknap		24-Apr-2001		SPR 73720	Sync (1.0)
*	Fix menu_transfer calls.
*
%^	Diana Pacelle	29-Jan-2002		SPR 62090, 80696
%^		Added operator privlege checking that was missing.  The
%^		permissions are ACE/AUTEN and ACE/AUTDS.  The VAX used SYSAD.
%^
%^   C. Hansen      19-Apr-2002               SPR 59856
%^       comment out paragraph V100_REJECT_screen it's not referenced.
%^	Penny Godfrey Beer 21-Apr-2003		spr #95545
%^	Remove update to last_login date/time stamp. _
%^
%^	C. McKenzie	18-Jul-2003		spr 81318
%^	Add functionality to restrict privileges for authorizing an operator
%^	down to the bank and location level if the configuration flag,
%^	OPR_REST_LOC is set, = Y.
%^
%^ T. Welch	25-May-2005	115377
%^	When grab location, use menu_opr_reg.curr_loc rather
%^	than the loc on menu_opr_union since MENU_CBK updates menu_opr_reg.
%^	Also use menu_opr_reg.curr_bnk rather than bank on menu_opr_union.
%^
%^
%^	Diana Pacelle	23-Aug-2005		SPR 125335
%^	Could not delete operator using the OPR function, still logged in.
%^	The operator closed the window and did not log out.
%^	The process is gone but the menu_signon_index still has the
%^	entry.  Changed to remove the entry when the process has disappeared.
%^
%^
%^ D. Pacelle	16-Oct-2005	125405
%^	Fixed Object lock wait when trying to enable an operator that was
%^	disabled when the inactive days count had been exceeded and
%^	the operator's screen was still active. (also changed menu_lgn)
*******************************************************************************

%def	<ACE>		%`SBJ_DD_PATH:ACE_FSECT.DDL`		%end


%def			%^ local fsect.

Aut_Opr_index:		que(	%`SBJ_DD_PATH:OPR_INDEX.DDF`);
Aut_opr_union:		set(	%`SBJ_DD_PATH:OPR_UNION.DDF`);
Aut_opr_reg:		reg(	%`SBJ_DD_PATH:OPR_REG.DDF`);
Aut_signon_index:	que(	%`SBJ_DD_PATH:MENU_SIGNON_INDEX.DDF`);
CHange_log:		que(	%`SBJ_DD_PATH:OPR_ACTION_LOG.DDF`);
Menu_aut_set:		set(	%`SBJ_DD_PATH:MENU_AUT_SET.DDF`);
Call_status_wf:		boolean;
cfg_opr_rest_loc_ws:	str(1);
Killing_opr_id_ws:	vstr(10);
Killed_opr_id_ws:	vstr(10);
Kill_process_id_ws:	long;
Kill_status_wf:		long;
Select_function_wf:     oneof(  %`SBJ_DD_PATH:DAT_FUNCTION_ONEOF.DDF`);
ws_rsts:		boolean;
Nex_errno_oneof_ws:	ONEOF(	%`SBJ_DD_PATH:NEX_ERRNO_ONEOF.DDF`);

%^ Screens
	%`SBJ_DD_PATH:MENU_AUT_SCR.DEF`


%^ The value of this variable determines whether we are to stay or go.
Menu_next_function_ws:	rec(
 Product_id:		vstr(3);
 Function_id:		vstr(6); );


Next_action_wf:		oneof(stay_here_is, timeout_is, menu_is);

first_time_sw:		oneof(first_time_thru,been_there);

Menu_Parameter:		VSTR(4);

Has_enable_priv:	boolean;
Has_disable_priv:	boolean;
Temp_memo_buff:	vstr(%`%ACE$_MSG_STR_SIZE`);	
parse:			parse;
compose:		compose;
Subject_sts:		boolean;

Clear_index_anyway:	boolean;

%end

%Procedure.
A100_MAIN.

	Set stay_here_is in Next_action_wf to true.
	Set first_time_thru in first_time_sw to true.

	Perform B000_SUBJECT_INIT thru B000_SUBJECT_INIT_END.

	Perform C000_AUT_SELECT THRU C000_AUT_SELECT_END 
		until not stay_here_is in Next_action_wf.


	IF (Timeout_Is IN Next_Action_Wf)
	THEN
	    %BEG Menu_Parameter = "*TO*"; %END
	ELSE
	    %BEG Menu_Parameter = null; %END
	END-IF.

	CALL "MENU_TRANSFER" USING
	  BY REFERENCE Menu_Parameter.

	%^ clean up before exit
	Perform B050_AUT_BREAK thru B050_AUT_BREAK_END.

A100_MAIN_END.
	%EXIT PROGRAM.

******************************************************************************
*
* This paragraph init's the program's subjects.
*
B000_SUBJECT_INIT.

        Call "DAT_CONN_ROOT". 
                              
        %ACE_CONN_Q "MTS"////"CHNG_AUT_LOG" to Change_log giving ws_rsts; 
	If failure_is in ws_rsts 

		CALL "NEX_CREATE_AND_FORMAT_MSG" USING 
		    BY VALUE %`%ACE$_MSG_STR_SIZE`
		    BY REFERENCE Temp_memo_buff
		    BY REFERENCE Temp_memo_buff_length
		    by content Z"MENU_AUT$_NOAUTLOG"
		    BY VALUE -1 
		    %ace_msg_arg_list();
		  RETURNING ws_rsts

		If success_is in ws_rsts
		%^ Parse past the message id. 	
		    %beg
		    parse ^IN(temp_memo_buff)
			^STRING, " - ", menu_msg1, /;
		    %end
		else
		%^ just pass the information as is and the message id 
		%beg
		compose ^out(menu_msg1), "MENU_AUT$_NOAUTLOG", / ;
		%end
		End-if
	end-if.



%^ connect to the /MENU_SIGNON_SEQ 
%^ note: menu_name_seq, menu_function_index,
%^ and menu_signon_seq are already connected.

	%beg
	Dat_root_set
		(.opr_index CONN: AUT_OPR_INDEX);

	menu_next_function_ws = menu_next_function;

	FIRST: Menu_priv_seq;
	Menu_priv_seq ^SEARCH Key="ACEAUTEN";		
	Has_enable_priv = Menu_priv_seq STATUS;		%^ Enable privilege

	FIRST: Menu_priv_seq;
	Menu_priv_seq ^SEARCH Key="ACEAUTDS";		
	Has_disable_priv = Menu_priv_seq STATUS;	%^ Disable privilege

	%end.


B000_SUBJECT_INIT_END.
	Exit.

B050_AUT_BREAK.
	%beg
	BREAK: AUT_OPR_INDEX;
	BREAK: Menu_Aut_Set;
	BREAK: Menu_Aut_Scr;
	BREAK: Aut_Opr_Union;
	BREAK: Aut_Opr_Reg;
	%end.

B050_AUT_BREAK_END.
	Exit.


C000_AUT_SELECT.
%^ The select screens are used to select a Database Maintenance subfunction 
%^ and an opr union.  The previous union connections (if any) are broken 
%^ and new ones are made before passing control to the data page screen.  

%^ Reject old screen and allocate a new one.

	If first_time_thru 
	then
		perform V000_ALLOC_SCREEN thru V000_ALLOC_SCREEN_END

	 	%beg
		Menu_aut_scr(
			.Fkeys(
			  .Timout.enable = T,
			  .Goldcancel.enable = T,
			  .Goldcancel.noedit = T,
			  .Entr.enable = T),
			.Cmds(
		  	    .Cmd_menu.enable = T,
		  	    .Cmd_menu.Noedit = T ),


		  	.Msg1 = MENU_MSG1,
		  	.Msg2 = MENU_MSG2 );

			Menu_msg1 = null;
			Menu_msg2 = null;

			SEND: Menu_aut_SCR(
			.Menu_aut_set send == Menu_aut_set );
		%end

		Set Been_there in first_time_sw to true
	Else
	  %beg
		Menu_aut_scr.msg1 = Menu_msg1;
		Menu_aut_scr.msg2 = Menu_msg2;
		Menu_msg1 = null;
		Menu_msg2 = null;
		
		reply:menu_aut_scr &;
		reply:menu_aut_set;

	  %end
	End-if.


	


%^******************************************************************************

C000_DISPATCH.

	Evaluate true
	  when scr_status  in Menu_aut_scr = "TIMOUT"
		%beg 
		menu_next_function_ws.function_id = "*TO*";
		Menu_msg1 = "VMSG$_TIMOUT";
		%end
		Set timeout_is in next_action_wf to true

	  when scr_status of Menu_aut_scr = "GOLDCANCEL"
		%beg
		menu_aut_scr.Cmdarg = Null;
		%end

		call "MENU_PARSE" using 
		 by reference Cmdarg of Menu_aut_scr
		 returning Call_status_wf

		Set menu_is in next_action_wf to true

	  when scr_status of Menu_aut_scr =  "CMD_MENU"

		call "MENU_PARSE" using
		 by reference Cmdarg of Menu_aut_scr
		 returning Call_status_wf

		if failure_is in Call_status_wf
		  then	%beg
			Menu_Msg2 = Menu_errmsg;
			%end
			go to C000_AUT_SELECT_END
		end-if

		set menu_is in Next_action_wf to true


	  when scr_status of Menu_aut_scr = "ENTR"
		Perform D100_edit_screen thru d100_edit_screen_end


	  when other

		%beg MENU_MSG2 = "VMSG$_INVLD_KEY_CMD"; %end
		go to C000_AUT_SELECT_END
	end-evaluate.

C000_AUT_SELECT_END.
	Exit.


D100_EDIT_SCREEN.
	
	%beg
	BREAK: Aut_Opr_union;
	Aut_Opr_index(read_only, Key = Menu_aut_set.Operator_id,
	^SEARCH CONN: Aut_Opr_union);

	%end

	If failure_is in Aut_opr_index_status


		then
		%beg
		Menu_Msg1 = "VMSG$_DAT_NOKEY";
		%end
		go to D100_EDIT_SCREEN_END
	end-if

	%beg
	BREAK: Aut_Opr_Reg;
	Aut_opr_union	( Reg: Aut_Opr_reg); %end.

	Call "GCV_OPR_REST_LOC" using
	    by reference cfg_opr_rest_loc_ws.

	evaluate The_func of Menu_aut_set

	%^ Enable.
	  when "E"

		If Failure_is in Has_enable_priv
			%beg
			Menu_Msg2 = "MSG$_OPR_NOTENAPRIV";
			%end
			go to D100_EDIT_SCREEN_END
		End-if

	   	if (Enabled_flag of Aut_opr_reg  = "Y")
			%beg
			Menu_Msg2 = "MSG$_OPR_NOTDIS";
			%end
			go to D100_EDIT_SCREEN_END
	   	end-if

		If cfg_opr_rest_loc_ws = "Y"
		    If curr_bnk of menu_opr_reg not = idbank of opr_owner of aut_opr_index or
		       curr_loc of menu_opr_reg not = idloc of opr_owner of aut_opr_index
			%beg Menu_Msg2 = "VMSG$_DAT_NOKEY"; %end
		    	go to D100_EDIT_SCREEN_END
		    End-if
	   	end-if

	   	%beg
		Aut_opr_reg(notrap, nomod_wait, mod);
		subject_sts = Aut_opr_reg status;
		%end

		If Failure_is in subject_sts then
			%beg Menu_Msg2 = "MSG$_OPR_LOCKED"; %end
		    	go to D100_EDIT_SCREEN_END
		End-if

*                %beg Change_Log.Systime now; %end
		 set add_is in Select_function_wf to true
       		%beg
                Alloc_Elem: Change_Log(notrap,
		 .systime NOW,
                 .Person = Menu_Opr_Union.Opr_Login_Id,
                 .File_Type = "AUT",
                 .Dat_Func  = select_function_wf,
                 .file_key = Menu_aut_set.Operator_id,
                 .Memo = Null );
		%end 

	   	%beg
	   	Aut_opr_reg.Enabled_flag = "Y";
	   	COMMIT: Tran;
		Aut_opr_reg(etrap, nomod);
	   	Menu_Msg2 = "MSG$_OPR_SUCENA";
	   	%end


	%^ Disable.
	   when "D"
		If Failure_is in Has_disable_priv
			%beg
			Menu_Msg2 = "MSG$_OPR_NOTDISPRIV";
			%end
			go to D100_EDIT_SCREEN_END
		End-if

	   	if (not (Enabled_flag in Aut_opr_reg = "Y"))
			%beg
			Menu_Msg2 = "MSG$_OPR_NOTENA";
			%end
			go to D100_EDIT_SCREEN_END
		end-if

		If cfg_opr_rest_loc_ws = "Y"
		    If curr_bnk of menu_opr_reg not = idbank of opr_owner of aut_opr_index or
			curr_loc of menu_opr_reg not = idloc of opr_owner of aut_opr_index
			%beg Menu_Msg2 = "VMSG$_DAT_NOKEY"; %end
		    	go to D100_EDIT_SCREEN_END
		    End-if
	   	end-if

		%beg
		Aut_opr_reg(notrap, nomod_wait, mod);
		subject_sts = Aut_opr_reg status;
		%end

		If Failure_is in subject_sts then
			%beg Menu_Msg2 = "MSG$_OPR_LOCKED"; %end
		    	go to D100_EDIT_SCREEN_END
		End-if

*                %beg Change_Log.Systime now; %end
		 set delete_is in Select_function_wf to true
       		%beg
                Alloc_Elem: Change_Log(notrap,
		 .systime NOW,
                 .Person = Menu_Opr_Union.Opr_Login_Id,
                 .File_Type = "AUT",
                 .Dat_Func  = select_function_wf,
                 .file_key = Menu_aut_set.Operator_id,
                 .Memo = Null );
		%end 

		%beg
	   	Aut_opr_reg.Enabled_flag = "N";
	   	COMMIT: Tran;
		Aut_opr_reg(etrap, nomod);
	   	Menu_Msg2 = "MSG$_OPR_SUCDIS";
	   	%end

	%^ Reset.
	   when "R"
		If Failure_is in Has_enable_priv
			%beg
			Menu_Msg2 = "MSG$_OPR_NOTENAPRIV";
			%end
			go to D100_EDIT_SCREEN_END
		End-if

		If cfg_opr_rest_loc_ws = "Y"
		    If curr_bnk of menu_opr_reg not = idbank of opr_owner of aut_opr_index or
			curr_loc of menu_opr_reg not = idloc of opr_owner of aut_opr_index
			%beg Menu_Msg2 = "VMSG$_DAT_NOKEY"; %end
		    	go to D100_EDIT_SCREEN_END
		    End-if
	   	end-if

		Perform E100_RESET_OPR thru E100_RESET_OPR_END
		%beg
		BREAK: Aut_signon_index;
		%end

	end-evaluate.


D100_EDIT_SCREEN_end.
	Exit.


E100_RESET_OPR.
%^ Reset the operator.
%^ The menu operator has requested that we reset the
%^ operator whose login id was entered on the AUT screen.
%^ This means we kill the process associated with the
%^ operator that is being reset and we remove the operator's
%^ entry from the Menu_signon_index.
%^ The question may arise, how do we know the process ID of
%^ the operator that is being reset?  That information is
%^ stored in operator's entry in the menu_signon_index.

	%beg
	BREAK: Aut_signon_index;
	Menu_signon_index EQUATE: Aut_signon_index (read_only);
	SEARCH: Aut_signon_Index (notrap,
		Key = Menu_aut_set.Operator_id);
	%End.

	If failure_is in Aut_signon_index_status
		%beg
		Menu_Msg2 = "MSG$_OPR_NOTRESET_NOTSGNDON";
		%end
		go to E100_RESET_OPR_END
	end-if.

%^ Prevent operator from killing his own session

	%beg
	Killed_opr_id_ws  = Menu_aut_set.Operator_id;
	Killing_opr_id_ws = Menu_opr_union.opr_login_id;
	%end

	If Killing_opr_id_ws_length = Killed_opr_id_ws_length and	
	   Killing_opr_id_ws (1:killing_opr_id_ws_length) =
	   Killed_opr_id_ws
		%beg
		Menu_Msg2 = "MSG$_OPR_NOTRESET_NOSUICIDE";
		%end
		Go to E100_RESET_OPR_end
	End-if.

	%Beg
	Kill_process_id_ws = Aut_signon_index.Opr_process_id;
	%End.

	Set Failure_is in Clear_index_anyway to true.

	If Kill_process_id_ws not > 0
		%beg
		Menu_Msg2 = "MSG$_OPR_NOTRESET_NOPID";
		%end
		go to E100_RESET_OPR_END
	End-if.
		
	call "kill" using by value Kill_process_id_ws
			  by value 15
		    returning Kill_status_wf.
		
	%^ Did we kill the process?
		
	If Kill_status_wf not = 0
		Call "NEX_GET_ERRNO" Returning Nex_errno_oneof_ws
		Evaluate TRUE
			When EINVAL in Nex_errno_oneof_ws
				%beg
				Menu_Msg2 = "MSG$_OPR_NOTRESET_EINVAL";
				%end
			When EPERM in Nex_errno_oneof_ws
				%beg
				Menu_Msg2 = "MSG$_OPR_NOTRESET_EPERM";
				%end
			When ESRCH in Nex_errno_oneof_ws
				%beg
				Menu_Msg2 = "MSG$_OPR_NOTRESET_ESRCH";
				%end
				Set Success_is in Clear_index_anyway to true		%^ probably closed window by hitting X
			When Other
				%beg
				Menu_Msg2 = "MSG$_OPR_NOTRESET";
				%end
		End-Evaluate
		If Failure_is in Clear_index_anyway
			go to E100_RESET_OPR_END
		End-if
	End-if.

%^ sleep for a bit to let Entia cleanup the process

	call "sleep" using by value 3.

	%^ Delete operator's entry from the Menu_signon_index
	%Beg
	DELETE: Aut_signon_index (notrap, insert_lock, nomod_wait);
	%End.
	
	If success_is in Aut_Signon_Index_status
		%beg
		COMMIT: Tran;
		Menu_Msg2 = "MSG$_OPR_SUCRESET";
		%end
		call "NEX_CREATE_AND_BROADCAST_MSG" using
			by content Z"MENU_AUT$_OPR_RESET"
			by value -1
			%ace_msg_arg_list(Killed_opr_id_ws,
					  Killing_opr_id_ws);
			returning ws_rsts


*                %beg Change_Log.Systime now; %end
		 set update_is in Select_function_wf to true
       		%beg
                Alloc_Elem: Change_Log(notrap,
		 .systime NOW,
                 .Person = Menu_Opr_Union.Opr_Login_Id,
                 .File_Type = "AUT",
                 .Dat_Func  = select_function_wf,
                 .file_key = Menu_aut_set.Operator_id,
                 .Memo = Null );
		
		commit: Tran;
		
		%end 

	Else
		%beg
		Menu_Msg2 = "MSG$_OPR_NOTRESET_NOTDEL";
		%end
	End-if.

E100_RESET_OPR_END.
	EXIT.

%^******************************************************************************

V000_ALLOC_SCREEN.

	%beg

	ALLOC_TEMP:	Menu_aut_set(mod);
	Alloc_temp:	Menu_aut_scr;

	%end.


V000_ALLOC_SCREEN_END.
	Exit.




* Reject Menu_aut and Menu_aut_set.
* the following paragraph is never referenced so comment it out
*v100_REJECT_screen.
*	%beg
*	BREAK: Menu_aut_scr ;
*	BREAK: Menu_aut_set;
*	%end.
*v100_REJECT_screen_end.
*	Exit.
