# Functional Specification: Enhancement to ZLOG_HIST_SYN Report

## Document Information
| Attribute | Details |
|-----------|---------|
| **Program Name** | ZLOG_HIST_SYN |
| **Enhancement Type** | Input Validation & User Confirmation |
| **Author** | Bibhuti Padhan |
| **Creation Date** | 20-01-2026 |
| **SAP Release** | ECC 6.0 / NetWeaver 7.31 |
| **Change Request** | [To be assigned] |

---

## 1. Overview

### 1.1 Purpose
This document describes the functional requirements for enhancing the existing ZLOG_HIST_SYN report to improve data integrity and prevent accidental data updates by implementing mandatory field validations and user confirmation mechanisms.

### 1.2 Background
The ZLOG_HIST_SYN report currently allows execution with missing critical input parameters (Area, Reporting Number, Item Number), which can lead to:
- Unintended mass updates to database tables
- Data integrity issues
- Accidental execution of History synchronization affecting YTTSTX0002 table

### 1.3 Business Objective
- **Improve Data Quality**: Ensure all critical parameters are provided before execution
- **Prevent Errors**: Stop execution when mandatory fields are missing
- **User Safety**: Add confirmation prompt before executing History synchronization
- **Audit Trail**: Track all History synchronization executions for compliance
- **Accountability**: Maintain record of who executed History sync and when
- **Compliance**: Enforce input validation regardless of configuration table settings

---

## 2. Current System Behavior

### 2.1 Existing Validation Logic
Currently, the program validates mandatory fields only when `ZLOG_EXEC_VAR` table has an active entry for 'ZLOG_HIST_SYN_INPUT_VALID'. This makes validation conditional rather than mandatory.

**Current Validation Code Pattern:**
```abap
AT SELECTION-SCREEN ON p_area.
  IF lw_zlog_exec_var IS NOT INITIAL.
    IF p_area IS INITIAL.
      MESSAGE 'Enter Area' TYPE 'E'.
    ENDIF.
  ENDIF.
```

### 2.2 Program Options
The report has three radio button options:
1. **p_corr (Correction)**: Uses p_area, p_rep_c, s_item, p_tknum
2. **p_retr (Retrieve)**: Uses p_area, s_report, s_item
3. **p_hist (History)**: Uses p_area, s_report, s_item - Updates YTTSTX0002 from history data

### 2.3 Current Issues
| Issue ID | Description | Business Impact |
|----------|-------------|-----------------|
| ISS-01 | Area can be empty during execution | Unfiltered database access |
| ISS-02 | Reporting Number can be empty | Mass updates possible |
| ISS-03 | Item Number can be empty | Unintended data scope |
| ISS-04 | Shipment Number can be empty in Correction mode | Silent execution failure, no user feedback |
| ISS-05 | No confirmation for History option | Accidental data updates |
| ISS-06 | No audit trail for History executions | Cannot track who executed History sync and when |
| ISS-07 | Validation depends on config table | Inconsistent behavior |

---

## 3. Proposed Solution

### 3.1 Mandatory Field Validation

#### 3.1.1 Area Field (p_area)
- **Field**: p_area (Type: YAREA)
- **Requirement**: Must be mandatory for ALL radio button options
- **Validation Point**: AT SELECTION-SCREEN ON p_area
- **Error Message**: "Area is mandatory. Please enter Area"
- **Message Type**: Error (E) - stops execution

#### 3.1.2 Reporting Number (p_rep_c / s_report)
**For Correction Option (p_corr):**
- **Field**: p_rep_c (Type: YREPORT_NO)
- **Requirement**: Must be mandatory when p_corr is selected
- **Validation Point**: AT SELECTION-SCREEN ON p_rep_c
- **Error Message**: "Reporting No is mandatory. Please enter Reporting No"
- **Message Type**: Error (E)

**For Retrieve/History Options (p_retr / p_hist):**
- **Field**: s_report (Range table for YREPORT_NO)
- **Requirement**: Must be mandatory when p_retr or p_hist is selected
- **Validation Point**: AT SELECTION-SCREEN ON s_report
- **Error Message**: "Reporting No is mandatory. Please enter Reporting No"
- **Message Type**: Error (E)

#### 3.1.3 Item Number (s_item)
- **Field**: s_item (Range table for Item Number)
- **Requirement**: Must be mandatory for ALL radio button options
- **Validation Point**: AT SELECTION-SCREEN ON s_item
- **Error Message**: "Item No is mandatory. Please enter Item No"
- **Message Type**: Error (E)

#### 3.1.4 Shipment Number (p_tknum)
- **Field**: p_tknum (Type: TKNUM)
- **Requirement**: Must be mandatory when p_corr (Correction) is selected
- **Validation Point**: AT SELECTION-SCREEN ON p_tknum
- **Error Message**: "Shipment No is mandatory for Correction mode. Please enter Shipment No"
- **Message Type**: Error (E)
- **Note**: Currently, if p_tknum is empty in Correction mode, the program silently does nothing (lines 181-209). This validation prevents that silent failure.

### 3.2 User Confirmation for History Synchronization

#### 3.2.1 Confirmation Dialog
- **Trigger**: When p_hist radio button is selected and user executes (F8)
- **Validation Point**: AT SELECTION-SCREEN (general validation)
- **Dialog Type**: Confirmation popup using POPUP_TO_CONFIRM function module
- **Dialog Elements**:
  - **Title**: "Confirmation Required"
  - **Question**: "Are you sure you want to execute History Synchronization? This will update YTTSTX0002 table from history data."
  - **Button 1**: "Yes" - Proceed with execution
  - **Button 2**: "No" - Cancel execution
  - **Default**: "No" (safe default)
  - **Cancel Button**: Hidden (force user to choose)

#### 3.2.2 User Action Outcomes
| User Action | System Response | Message |
|-------------|----------------|---------|
| Click "Yes" | Proceed with history synchronization, log audit entry | None (continue execution) |
| Click "No" | Stop execution, return to selection screen | "Execution cancelled by user" (Type: S, Display like: E) |

### 3.3 Audit Trail Logging for History Synchronization

#### 3.3.1 Audit Table: YTTSA
- **Table Name**: YTTSA
- **Purpose**: Track all History synchronization executions
- **Update Timing**: After user confirms "Yes" on popup, before processing starts
- **Transaction**: Same transaction as History sync (rolled back if sync fails)

#### 3.3.2 Audit Fields

| Field Name | Data Element | Source | Description |
|------------|--------------|--------|-------------|
| AREA | YAREA | p_area | Area code from selection screen |
| REPORT_NO | YREPORT_NO | s_report-low | First reporting number from range |
| FUNCTION | YSTATS | 'HIST_SYNC' | Function identifier (constant) |
| EDITDT | DATUM | sy-datum | Execution date (system date) |
| EDITTM | UZEIT | sy-uzeit | Execution time (system time) |
| EDITBY | SYUNAME | sy-uname | User who executed (system user) |

#### 3.3.3 Audit Record Creation Logic
- **Trigger**: User clicks "Yes" on History confirmation popup
- **Action**: Insert single record to YTTSA table
- **Key**: AREA + REPORT_NO + FUNCTION + EDITDT + EDITTM + EDITBY
- **Error Handling**: If insert fails, log error but continue with History sync

#### 3.3.4 Audit Record Content

**Example Record:**
```
AREA:      100
REPORT_NO: REP001
FUNCTION:  HIST_SYNC
EDITDT:    20260120
EDITTM:    143530
EDITBY:    BIBHUTI
```

**Interpretation**: User BIBHUTI executed History Synchronization for Area 100, Report REP001 on 2026-01-20 at 14:35:30

#### 3.3.5 Business Value
- **Compliance**: Track all data modifications for audit purposes
- **Accountability**: Identify who executed History synchronization
- **Traceability**: Trace when specific synchronizations occurred
- **Troubleshooting**: Debug issues by reviewing execution history
- **Governance**: Monitor usage patterns and frequency

---

## 4. Business Rules

### 4.1 Validation Rules
| Rule ID | Description | Condition | Action |
|---------|-------------|-----------|--------|
| BR-01 | Area must always be provided | p_area IS INITIAL | Display error, stop execution |
| BR-02 | Report number required for Correction | p_corr = 'X' AND p_rep_c IS INITIAL | Display error, stop execution |
| BR-03 | Report number required for Retrieve/History | (p_retr = 'X' OR p_hist = 'X') AND s_report IS INITIAL | Display error, stop execution |
| BR-04 | Item number must always be provided | s_item IS INITIAL | Display error, stop execution |
| BR-05 | Shipment number required for Correction | p_corr = 'X' AND p_tknum IS INITIAL | Display error, stop execution |
| BR-06 | Confirmation required for History sync | p_hist = 'X' | Display confirmation popup |
| BR-07 | Audit trail required for History sync | p_hist = 'X' AND user confirms | Insert record to YTTSA |
| BR-08 | Validation independent of config table | Always | Remove dependency on lw_zlog_exec_var |

### 4.2 User Confirmation Rules
| Rule ID | Description | Implementation |
|---------|-------------|----------------|
| UC-01 | Confirmation only for History option | IF p_hist = abap_true |
| UC-02 | Default selection is "No" | default_button = '2' |
| UC-03 | Cancel action stops execution | LEAVE LIST-PROCESSING |
| UC-04 | Popup displayed before data processing | AT SELECTION-SCREEN event |
| UC-05 | Audit entry created after confirmation | After user clicks "Yes", before START-OF-SELECTION |
| UC-06 | Audit uses HIST_SYNC as function code | Constant identifier for History sync |
| UC-07 | Audit captures first report number from range | s_report-low value used |

---

## 5. User Interface Changes

### 5.1 Error Messages
No changes to selection screen layout. New/updated error messages:

| Message ID | Message Text | Type | Display Condition |
|------------|--------------|------|-------------------|
| MSG-001 | Area is mandatory. Please enter Area | E | p_area IS INITIAL |
| MSG-002 | Reporting No is mandatory. Please enter Reporting No | E | Reporting field IS INITIAL |
| MSG-003 | Item No is mandatory. Please enter Item No | E | s_item IS INITIAL |
| MSG-004 | Shipment No is mandatory for Correction mode. Please enter Shipment No | E | p_corr = 'X' AND p_tknum IS INITIAL |
| MSG-005 | Execution cancelled by user | S (Display like E) | User clicks "No" on confirmation |

### 5.2 Confirmation Dialog
**Dialog Appearance:**
```
┌──────────────────────────────────────────────────────────┐
│ Confirmation Required                                     │
├──────────────────────────────────────────────────────────┤
│                                                           │
│ Are you sure you want to execute History                 │
│ Synchronization? This will update YTTSTX0002 table       │
│ from history data.                                        │
│                                                           │
│              [    No    ]    [   Yes   ]                 │
│                                                           │
└──────────────────────────────────────────────────────────┘
```

---

## 6. Impact Analysis

### 6.1 Process Impact
| Process | Current Behavior | New Behavior | Impact Level |
|---------|------------------|--------------|--------------|
| Correction Mode | Can run without Area/Item | Blocked if missing | **High** - Users must provide all fields |
| Retrieve Mode | Can run without Area/Report/Item | Blocked if missing | **High** - Users must provide all fields |
| History Mode | Can run without confirmation | Requires confirmation | **Medium** - Extra step for safety |
| Config-based validation | Inconsistent | Always enforced | **High** - Always consistent |

### 6.2 User Impact
- **Users must provide all mandatory fields** before execution (previously optional in some cases)
- **History synchronization requires confirmation** (new extra step)
- **No workaround** - validation always enforced (cannot bypass)
- **Improved safety** - prevents accidental mass updates

### 6.3 Data Impact
- **Reduced risk** of unintended database updates
- **Improved data quality** by ensuring filtered execution
- **No impact on existing data** - only affects future executions

---

## 7. Error Scenarios

### 7.1 Validation Errors
| Scenario | User Action | System Response |
|----------|-------------|-----------------|
| User executes without Area | Press F8 with empty p_area | Error: "Area is mandatory. Please enter Area" |
| User executes Correction without Report | Press F8 with p_corr selected, p_rep_c empty | Error: "Reporting No is mandatory. Please enter Reporting No" |
| User executes Retrieve without Report | Press F8 with p_retr selected, s_report empty | Error: "Reporting No is mandatory. Please enter Reporting No" |
| User executes without Item | Press F8 with empty s_item | Error: "Item No is mandatory. Please enter Item No" |
| User executes Correction without Shipment | Press F8 with p_corr selected, p_tknum empty | Error: "Shipment No is mandatory for Correction mode. Please enter Shipment No" |

### 7.2 Confirmation Scenarios
| Scenario | User Action | System Response |
|----------|-------------|-----------------|
| User clicks "Yes" on confirmation | All validations passed, p_hist selected, click Yes | Proceed with history synchronization |
| User clicks "No" on confirmation | All validations passed, p_hist selected, click No | Message: "Execution cancelled by user", return to screen |

---

## 8. Success Criteria

### 8.1 Functional Success Criteria
- [ ] Program blocks execution when Area is missing
- [ ] Program blocks execution when Reporting Number is missing
- [ ] Program blocks execution when Item Number is missing
- [ ] Program blocks execution when Shipment Number is missing (Correction mode)
- [ ] Validation works regardless of ZLOG_EXEC_VAR table configuration
- [ ] Confirmation popup appears when History option is selected
- [ ] Confirmation popup has "No" as default selection
- [ ] User can proceed when clicking "Yes" on confirmation
- [ ] Execution stops when clicking "No" on confirmation
- [ ] Audit record created in YTTSA when History sync executes
- [ ] Audit record contains correct Area, Report Number, Function, Date, Time, User
- [ ] No audit record created when user cancels
- [ ] Error messages are clear and user-friendly

### 8.2 Non-Functional Success Criteria
- [ ] No performance degradation
- [ ] Validation occurs before database access
- [ ] User interface remains responsive
- [ ] Code follows ABAP coding standards for NetWeaver 7.31
- [ ] Code passes Code Inspector without errors/warnings

---

## 9. Testing Requirements

### 9.1 Functional Test Cases

#### Test Case Group 1: Mandatory Field Validation
| Test ID | Description | Test Steps | Expected Result |
|---------|-------------|------------|-----------------|
| FT-001 | Validate Area mandatory | 1. Leave p_area empty<br>2. Fill other fields<br>3. Execute | Error: "Area is mandatory. Please enter Area" |
| FT-002 | Validate p_rep_c mandatory for Correction | 1. Select p_corr<br>2. Leave p_rep_c empty<br>3. Fill p_area, s_item<br>4. Execute | Error: "Reporting No is mandatory. Please enter Reporting No" |
| FT-003 | Validate s_report mandatory for Retrieve | 1. Select p_retr<br>2. Leave s_report empty<br>3. Fill p_area, s_item<br>4. Execute | Error: "Reporting No is mandatory. Please enter Reporting No" |
| FT-004 | Validate s_report mandatory for History | 1. Select p_hist<br>2. Leave s_report empty<br>3. Fill p_area, s_item<br>4. Execute | Error: "Reporting No is mandatory. Please enter Reporting No" |
| FT-005 | Validate Item mandatory | 1. Select any option<br>2. Leave s_item empty<br>3. Fill other fields<br>4. Execute | Error: "Item No is mandatory. Please enter Item No" |
| FT-005a | Validate Shipment No mandatory for Correction | 1. Select p_corr<br>2. Leave p_tknum empty<br>3. Fill other fields<br>4. Execute | Error: "Shipment No is mandatory for Correction mode. Please enter Shipment No" |
| FT-006 | Validate all fields filled - Correction | 1. Select p_corr<br>2. Fill all mandatory fields (including p_tknum)<br>3. Execute | No validation error, proceed to execution |
| FT-007 | Validate all fields filled - Retrieve | 1. Select p_retr<br>2. Fill all mandatory fields<br>3. Execute | No validation error, proceed to execution |
| FT-008 | Validate all fields filled - History | 1. Select p_hist<br>2. Fill all mandatory fields<br>3. Execute | Confirmation popup appears |

#### Test Case Group 2: Configuration Independence
| Test ID | Description | Test Steps | Expected Result |
|---------|-------------|------------|-----------------|
| FT-009 | Validation with config inactive | 1. Ensure ZLOG_EXEC_VAR has no active entry<br>2. Leave p_area empty<br>3. Execute | Error: "Area is mandatory. Please enter Area" |
| FT-010 | Validation with config active | 1. Ensure ZLOG_EXEC_VAR has active entry<br>2. Leave p_area empty<br>3. Execute | Error: "Area is mandatory. Please enter Area" |

#### Test Case Group 3: User Confirmation
| Test ID | Description | Test Steps | Expected Result |
|---------|-------------|------------|-----------------|
| FT-011 | Confirmation popup appears | 1. Select p_hist<br>2. Fill all fields<br>3. Execute | Confirmation popup with title "Confirmation Required" |
| FT-012 | Confirmation default is "No" | 1. Trigger confirmation popup<br>2. Check default button | "No" button is highlighted by default |
| FT-013 | User confirms "Yes" | 1. Trigger confirmation popup<br>2. Click "Yes" | History synchronization proceeds |
| FT-014 | User confirms "No" | 1. Trigger confirmation popup<br>2. Click "No" | Message: "Execution cancelled by user", return to screen |
| FT-015 | Confirmation only for History | 1. Select p_corr<br>2. Fill all fields<br>3. Execute | No confirmation popup, direct execution |
| FT-016 | Confirmation only for History (Retrieve) | 1. Select p_retr<br>2. Fill all fields<br>3. Execute | No confirmation popup, direct execution |
| FT-017 | Audit trail created for History sync | 1. Select p_hist<br>2. Fill all fields<br>3. Execute<br>4. Click "Yes" on confirmation<br>5. Check YTTSA table | Record inserted with AREA, REPORT_NO, FUNCTION='HIST_SYNC', EDITDT, EDITTM, EDITBY |
| FT-018 | No audit trail when cancelled | 1. Select p_hist<br>2. Fill all fields<br>3. Execute<br>4. Click "No" on confirmation<br>5. Check YTTSA table | No record inserted |

### 9.2 Integration Test Cases
| Test ID | Description | Expected Result |
|---------|-------------|-----------------|
| IT-001 | Execute Correction mode with valid data | Updates performed successfully |
| IT-002 | Execute Retrieve mode with valid data | Data retrieved successfully |
| IT-003 | Execute History mode with confirmation | YTTSTX0002 table updated from history, YTTSA audit record created |
| IT-004 | Verify audit trail in YTTSA | YTTSA contains execution record with correct user, date, time |

---

## 10. Dependencies

### 10.1 Technical Dependencies
- ABAP NetWeaver 7.31 function module: POPUP_TO_CONFIRM
- No new database tables required
- No new message classes required (using inline messages)

### 10.2 Data Dependencies
- Existing tables: YTTSTX0002, YTTSTX0002_HIST, YTTSTX0001, ZLOG_EXEC_VAR
- No changes to table structures

### 10.3 Configuration Dependencies
- **Removed dependency**: No longer depends on ZLOG_EXEC_VAR table for validation
- Validation is now unconditional and always active

---

## 11. Assumptions and Constraints

### 11.1 Assumptions
- Users have appropriate authorization to execute the report
- SAP system has POPUP_TO_CONFIRM function module available (standard in NetWeaver 7.31)
- Users understand the impact of History synchronization

### 11.2 Constraints
- Must maintain NetWeaver 7.31 compatibility (no 7.40+ syntax)
- Cannot change selection screen layout (only validation logic)
- Must not impact existing functionality beyond validation

---

## 12. Approval and Sign-off

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Business Owner | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| QA Lead | | | |

---

## 13. Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 20-01-2026 | Bibhuti Padhan | Initial Functional Specification |

---

**End of Functional Specification**

