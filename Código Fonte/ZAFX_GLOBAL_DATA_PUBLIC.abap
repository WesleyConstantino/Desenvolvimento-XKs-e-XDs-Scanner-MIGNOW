*----------------------------------------------------------------------*
*   INCLUDE AFX_GLOBAL_DATA_PUBLIC                                     *
*----------------------------------------------------------------------*

SET EXTENDED CHECK OFF.

* Global constants for archiving status
INCLUDE afx_global_archstat_public.


************************************************************************
*** CONSTANTS                                                        ***
************************************************************************

* Boolean: True or False
CONSTANTS:
  con_true                    TYPE afx_dte_boolean VALUE 'X',
  con_false                   TYPE afx_dte_boolean VALUE ' '.

* Answers from POPUP_TO_CONFIRM
CONSTANTS:
  g_con_confirm_yes(1)        TYPE c VALUE '1',
  g_con_confirm_no(1)         TYPE c VALUE '2',
  g_con_confirm_cancel(1)     TYPE c VALUE 'A'.

* Global constants for archiving mode
CONSTANTS:
  g_con_archive_mode_test     TYPE afx_dte_archmode
                              VALUE 'T',
*                                         " Start im Dialog
*                                         " Application Log im Dialog
*                                         " kein DB-Update erlaubt
  g_con_archive_mode_prod     TYPE afx_dte_archmode
                              VALUE 'P',
*                                         " Start im Batch
*                                         " Application Log im Batch
*                                         " DB-Update erlaubt
*                                         " Schreiben/Löschen durch ADK
  g_con_archive_mode_simu     TYPE afx_dte_archmode
                              VALUE 'S'.
*                                         " Start im Batch
*                                         " Application Log im Batch
*                                         " kein DB-Update erlaubt

* Global constants for analyze processing mode
CONSTANTS:
  g_con_analyze_mode_standard TYPE afx_dte_analyze_mode
                              VALUE 'S',
  g_con_analyze_mode_all      TYPE afx_dte_analyze_mode
                              VALUE 'A',
  g_con_analyze_mode_initial  TYPE afx_dte_analyze_mode
                              VALUE 'I',
  g_con_analyze_mode_progint  TYPE afx_dte_analyze_mode
                              VALUE 'P'.

* Global constants for archiving program types
CONSTANTS:
  g_con_progtype_prestep      TYPE afx_dte_prog_type VALUE 'PST',
  g_con_progtype_analyze      TYPE afx_dte_prog_type VALUE 'ANL',
  g_con_progtype_write        TYPE afx_dte_prog_type VALUE 'WRI',
  g_con_progtype_delete       TYPE afx_dte_prog_type VALUE 'DEL',
  g_con_progtype_reload       TYPE afx_dte_prog_type VALUE 'REL',
  g_con_progtype_terminator   TYPE afx_dte_prog_type VALUE 'TRM'.

* Global constants for minimum residence time and minimum
* package size/count in parallel processing
CONSTANTS:
  g_con_min_residence         TYPE afx_dte_residence_min VALUE     1,
  g_con_max_residence         TYPE afx_dte_residence_max VALUE 36500,
  g_con_max_follow_up         TYPE afx_dte_residence_max VALUE 36500,
  g_con_min_packagesize       TYPE afx_dte_packsize_min  VALUE  1000,
  g_con_min_packagecount      TYPE afx_dte_packcount_min VALUE     1.

* Global constants for archiving period units
CONSTANTS:
  g_con_period_unit_day       TYPE afx_dte_appllog_period_unit
                              VALUE '1',
  g_con_period_unit_week      TYPE afx_dte_appllog_period_unit
                              VALUE '2',
  g_con_period_unit_month     TYPE afx_dte_appllog_period_unit
                              VALUE '3',
  g_con_period_unit_year      TYPE afx_dte_appllog_period_unit
                              VALUE '4'.
CONSTANTS:
  g_con_daycount_day          TYPE i VALUE   1,
  g_con_daycount_week         TYPE i VALUE   7,
  g_con_daycount_month        TYPE i VALUE  31,
  g_con_daycount_year         TYPE i VALUE 365.

* Global constants for message log
CONSTANTS:
  g_con_msgid                 TYPE symsgid   VALUE 'AFX_GLOBAL',
  g_con_generic_msgno         TYPE symsgno   VALUE '000',
  g_con_pplog_subobject_info  TYPE balsubobj VALUE 'INFO'.

* Global contants for ADK exception handling
CONSTANTS:
  g_con_adk_fm_open_for_write     TYPE i VALUE 1,
  g_con_adk_fm_new_object         TYPE i VALUE 2,
  g_con_adk_fm_put_record         TYPE i VALUE 3,
  g_con_adk_fm_cd_archive_object  TYPE i VALUE 4,
  g_con_adk_fm_save_object        TYPE i VALUE 5,
  g_con_adk_fm_close_file         TYPE i VALUE 6,
  g_con_adk_fm_open_for_read      TYPE i VALUE 7,
  g_con_adk_fm_get_next_object    TYPE i VALUE 8,
  g_con_adk_fm_get_statistics     TYPE i VALUE 9,
  g_con_adk_fm_open_for_delete    TYPE i VALUE 10,
  g_con_adk_fm_get_cust_data      TYPE i VALUE 11,
  g_con_adk_fm_get_next_record    TYPE i VALUE 12,
  g_con_adk_fm_cd_del_arch_obj    TYPE i VALUE 13,
  g_con_adk_fm_cd_arch_get_table  TYPE i VALUE 14,
  g_con_adk_fm_give_statistics    TYPE i VALUE 15,
  g_con_adk_fm_get_archive_files  TYPE i VALUE 16,
  g_con_adk_fm_admin_set_status   TYPE i VALUE 17,
  g_con_adk_fm_get_information    TYPE i VALUE 18,
  g_con_adk_fm_open_for_move      TYPE i VALUE 19,
  g_con_adk_fm_save_objects       TYPE i VALUE 20,
  g_con_adk_fm_del_object_data    TYPE i VALUE 21,
  g_con_adk_fm_cd_rel_arch_obj    TYPE i VALUE 22,
  g_con_adk_fm_rel_object_data    TYPE i VALUE 23,
  g_con_adk_fm_rl_archive_object  TYPE i VALUE 24,
  g_con_adk_fm_rl_arch_get_table  TYPE i VALUE 25,

  g_con_adk_internal_error        TYPE i VALUE 1,
  g_con_adk_object_not_found      TYPE i VALUE 2,
  g_con_adk_open_error            TYPE i VALUE 3,
  g_con_adk_not_authorized        TYPE i VALUE 4,
  g_con_adk_others                TYPE i VALUE 5,
  g_con_adk_wrong_access_to_arch  TYPE i VALUE 6,
  g_con_adk_invalid_record_struc  TYPE i VALUE 7,
  g_con_adk_file_io_error         TYPE i VALUE 8,
  g_con_adk_termination_req       TYPE i VALUE 9,
  g_con_adk_file_already_open     TYPE i VALUE 10,
  g_con_adk_no_files_available    TYPE i VALUE 11,
  g_con_adk_end_of_file           TYPE i VALUE 12,
  g_con_adk_end_of_object         TYPE i VALUE 13,
  g_con_adk_cannot_change_status  TYPE i VALUE 14.

* Global constants for ADK job types
CONSTANTS:
  g_con_adk_jobtype_write         TYPE admi_job_t
                                  VALUE 'WRI',
  g_con_adk_jobtype_delete        TYPE admi_job_t
                                  VALUE 'DEL',
  g_con_adk_jobtype_reload        TYPE admi_job_t
                                  VALUE 'REL'.

* Global constants for authority check
CONSTANTS:
  g_con_auth_actvt_create         TYPE activ_auth VALUE '01',
*                  "used by following ADK-FBs:
*                  "     ARCHIVE_OPEN_FOR_WRITE
*                  "     ARCHIVE_OPEN_FOR_READ
*                  "     ARCHIVE_OPEN_FOR_MOVE
*                  "     ARCHIVE_OPEN_FOR_DELETE
*                  "     ARCHIVE_OPEN_FOR_CONVERSION
*  g_con_auth_actvt_update         TYPE activ_auth VALUE '02',
*                  "used by following ADK-FBs:
*                  "     not used
  g_con_auth_actvt_show           TYPE activ_auth VALUE '03'.
*                  "used by following ADK-FBs:
*                  "     ARCHIVE_OPEN_FOR_READ - "minimum" actvt

* Global contants for activity log status
CONSTANTS:
  g_con_actvt_arch_started
                 TYPE afx_dte_runstatus VALUE '00',
  g_con_actvt_pp_started
                 TYPE afx_dte_runstatus VALUE '10',
  g_con_actvt_package_created
                 TYPE afx_dte_runstatus VALUE '20',
  g_con_actvt_pp_ended
                 TYPE afx_dte_runstatus VALUE '30',
  g_con_actvt_writejob_started
                 TYPE afx_dte_runstatus VALUE '31',
  g_con_actvt_deletejob_started
                 TYPE afx_dte_runstatus VALUE '32',
  g_con_actvt_writejob_stop
                 TYPE afx_dte_runstatus VALUE '33',
  g_con_actvt_pp_crashed
                 TYPE afx_dte_runstatus VALUE '40',
  g_con_actvt_pp_terminated
                 TYPE afx_dte_runstatus VALUE '41',
  g_con_actvt_pp_startimpossible
                 TYPE afx_dte_runstatus VALUE '42',
  g_con_actvt_prog_normal_end
                 TYPE afx_dte_runstatus VALUE '50'.

* Global constants for status of parallel run:
CONSTANTS:
  g_con_run_undefined TYPE bank_dte_pp_runstatus
                      VALUE 0,
  g_con_processing    TYPE bank_dte_pp_runstatus
                      VALUE 1,                     "Bearbeitung läuft
  g_con_process_async TYPE bank_dte_pp_runstatus
                      VALUE 2,                     "asynchron aktiv
  g_con_run_error     TYPE bank_dte_pp_runstatus
                      VALUE 3,                     "Error
  g_con_continue      TYPE bank_dte_pp_runstatus
                      VALUE 4,                     "Fortsetzung
  g_con_pprocess      TYPE bank_dte_pp_runstatus
                      VALUE 5,                     "Nachbearbeitung
  g_con_run_finished  TYPE bank_dte_pp_runstatus
                      VALUE 6,                     "Ende des Laufs
  g_con_run_retry     TYPE bank_dte_pp_runstatus
                      VALUE 7.                     "Wiederholung

* Global contants for activity log package status
CONSTANTS:
  g_con_actvt_package_planed
                 TYPE afx_dte_package_status VALUE '00',
  g_con_actvt_package_in_work
                 TYPE afx_dte_package_status VALUE '10',
  g_con_actvt_package_finished
                 TYPE afx_dte_package_status VALUE '20',
  g_con_actvt_package_terminated
                 TYPE afx_dte_package_status VALUE '30'.

* Global constants used in package creation
CONSTANTS:
  g_con_guid_min TYPE guid_16 VALUE '00000000000000000000000000000000',
  g_con_guid_max TYPE guid_16 VALUE 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'.

* Global constants for archiving objects
CONSTANTS:
  g_con_object_all        TYPE objct_tr01 VALUE '*'.

* Global constant for missing description text
CONSTANTS:
  g_con_missing_langu_msg TYPE itex132
                          VALUE 'Description missing in language &'.


************************************************************************
*** VARIABLES                                                        ***
************************************************************************

* Global structure for global customizing
DATA:
  g_str_global_cust TYPE afx_str_global_cust.

* Global data for parallel processing
DATA:
  g_runid_ext       TYPE bank_dte_pp_runid_ext,
  g_package_no      TYPE bank_dte_pp_intervno,
  g_log_object      TYPE balobj_d,
  g_log_object_exc  TYPE balobj_d.

* Job information
DATA:
  g_str_jobkey      TYPE bank_str_jc_jobkey,
  g_selfjobname     TYPE btcjob,
  g_selfjobcount    TYPE btcjobcnt,
  g_selfstepcount   TYPE btcstepcnt.

* Variables for time measurement
DATA:
  g_timestamp_start TYPE timestamp,
  g_tmc_sy_datum    TYPE sydatum,
  g_tmc_sy_uzeit    TYPE syuzeit.

* Global flag for logging of runtime information
DATA:
  g_flg_runtime_log TYPE afx_dte_flg_runtime_log.

* Dynamic WHERE-Statement
DATA:
  g_tab_where_clause TYPE afx_tab_where_clause.

* Statistic information for analyze process
DATA:
  g_cnt_selected  TYPE sydbcnt,  " Anzahl selektierte Datensätze
  g_cnt_checked   TYPE sydbcnt,  " Anzahl geprüfte Datensätze
  g_cnt_follow_up TYPE sydbcnt,  " Anzahl Datensätze für Wiedervorlage
  g_cnt_expired   TYPE sydbcnt,  " Anzahl Datensätze mit abgelaufener
                                 " Residenzzeit
  g_cnt_arch      TYPE sydbcnt,  " Anzahl archivierbare Datensätze
  g_cnt_noarch    TYPE sydbcnt.  " Anzahl nicht archivierbare Datensätze

* Statistic information for write process
DATA:
  g_cnt_processed TYPE sydbcnt.      " Anzahl bearbeiteter Datensätze


* Variables for ADK
DATA:
  g_archive_document            TYPE admi_run_d,
  g_archive_handle              TYPE sy-tabix,
  g_tab_archive_files           TYPE afx_tab_archive_files,
  g_tab_arch_stat               TYPE afx_tab_arch_stati,
  g_tab_arch_stat_last          TYPE afx_tab_arch_stati,
  g_tab_statistics_data         TYPE afx_tab_statistics_data,
  g_flg_adk_termination_request TYPE afx_dte_boolean.

SET EXTENDED CHECK ON.
