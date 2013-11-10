;;; dkmisc.el --- Miscellaneous functions required by dk* packages.

;; Copyright: (c) David Keegan 2010-2013.
;; Licence: FSF GPLv3.
;; Author: David Keegan <dksw@eircom.net>
;; Version: 1.00
;; Package-Requires: ((emacs "24.1"))
;; Keywords: utility time date file
;; URL: https://github.com/davidkeegan/dkmisc

;;; Commentary:
;;
;; Miscellaneous elisp functions and definitions, including
;; time/date and string manipulation.
;;
;;; Code:

; DkTbd: Eliminate this?
(require 'org)

;;;###autoload
(defgroup dk nil
 "David Keegan's Packages."
 :tag "dk"
 :group 'emacs)

;;;###autoload
(defgroup dkmisc nil
 "Package dkmisc (misc elisp functions)."
 :tag "dkmisc"
 :group 'dk)

(defcustom dkmisc-ConflictMarkerRe "^<<<<<<"
 "Matches a merge conflict marker.
Conflict markers  are inserted by a version control system to indicate
where merge conflicts have arisen in the file."
;; :tag "dkmisc-ConflictMarkerRe"
 :type '(string))

(defun dkmisc-StringTrimWs(String)
 "Strip whitespace from the beginning and end of STRING."
 (let* (Result)
  (setq Result String)
  (and (string-match "^[ \t\n]*" Result)
   (setq Result (replace-match "" t t Result)))
  (and (string-match "[ \t\n]*$" Result)
   (setq Result (replace-match "" t t Result)))
  Result))

(defun dkmisc-DocstringGet(Symbol)
 "Return the documentation associated with SYMBOL, or nil if none."
 (interactive)
 ; Function?
 (if (fboundp Symbol)
   (documentation Symbol)
   ; Variable?
   (if (boundp Symbol)
    (documentation-property Symbol 'variable-documentation))))

(defun dkmisc-DocstringShowMatching(Pattern)
 "Extract the documentation text for symbols matching PATTERN.
Uses strict case matching."
 (interactive "sPattern: ")
 (let*
  (case-fold-search SymList)
  ; Sorted list of matching symbols.
  (mapatoms
   (lambda (Symbol)
    (let*
     ((Sn (symbol-name Symbol)))
     ; Matches Pattern?
     (if (string-match Pattern Sn)
       (push Symbol SymList)))))
  (setq SymList (sort SymList 'string<))

  ; Show in temporary buffer.
  (with-output-to-temp-buffer "*Package Documentation*"
   (dolist (Symbol SymList)
    (let*
     ((Sn (symbol-name Symbol))
      (Flag (make-string (- 60 (length Sn)) ?.)))
     (princ
      (format "\n\n\n%s %s%s\n%s"
       (if (fboundp Symbol) "Function" "Variable")
       Sn Flag (or (dkmisc-DocstringGet Symbol) "Not Documented!"))))))))

(defconst dkmisc-TwoDigits
 "[0-9]\\{2\\}"
 "Matches any two digits.")

(defconst dkmisc-YearRe
 (concat
  dkmisc-TwoDigits
  "?"
  dkmisc-TwoDigits)
 "Matches a two or four digit year.")

(defconst dkmisc-YearMonthRe
 (concat
  "\\("
  dkmisc-YearRe
  "\\)"
  "-?"
  "\\("
  dkmisc-TwoDigits
  "\\)")
 "Matches a Year and Month with optional punctuation.")

(defconst dkmisc-DateRe
 (concat
  dkmisc-YearMonthRe
  "-?"
  "\\("
  dkmisc-TwoDigits
  "\\)")
 "Matches an ISO date/time with optional punctuation.")

(defconst dkmisc-TimeRe
 (concat
  dkmisc-TwoDigits
  ":?"
  dkmisc-TwoDigits
  "\\("
  ":?"
  dkmisc-TwoDigits
  "\\)?")
 "Matches an ISO time with optional seconds and optional punctuation.")

(defconst dkmisc-DateTimeRe
 (concat
  "\\("
  dkmisc-DateRe
  "\\)"
  "\\("
  "[[:blank:]]"
  "\\("
  dkmisc-TimeRe
  "\\)"
  "\\)?")
 "Matches an ISO date/time with optional time and optional punctuation.")

(defun dkmisc-TimeZoneOffset(Seconds)
 "Return the timezone offset in seconds of UTC float time SECONDS."
 (car (current-time-zone (seconds-to-time Seconds))))

(defun dkmisc-TimeZoneLabel(Seconds)
 "Return the timezone label of UTC float time SECONDS."
 (nth 1 (current-time-zone (seconds-to-time Seconds))))

(defconst dkmisc-CurrentTimeShift nil
 "Time shift for test purposes in float seconds.
Set via `dkmisc-SetCurrentTimeShift'.")

(defun dkmisc-SetCurrentTimeShift (&optional DateTimeString)
 "Causes current-time to shift the value returned.
The value returned increments from the specified time. Call with
nil argument to eliminate the shift and return to normal.
WARNING: May have unexpected side-effects. Use for test purposes
only. Affects the appearance of the emacs cursor."
 ; Eliminate any existing time shift.
 (setq dkmisc-CurrentTimeShift nil)
 (if DateTimeString
  (let*
   ((Current (dkmisc-TimeCurrent))
    (Target (dkmisc-TimeParse DateTimeString)))
   (setq dkmisc-CurrentTimeShift (- Target Current))))
 dkmisc-CurrentTimeShift)

(defadvice current-time (after dkCt activate compile)
 "Shift the returned value for test purposes."
 (if dkmisc-CurrentTimeShift
  (let* ((Current (float-time ad-return-value)))
   (setq ad-return-value
    (seconds-to-time (+ Current dkmisc-CurrentTimeShift))))))

;;;###autoload
(defun dkmisc-DateToText(&optional Seconds Universal)
 "Convert the date part of float SECONDS to a string."
 (dkmisc-DateTimeToText Seconds "%Y-%m-%d" Universal))

;;;###autoload
(defun dkmisc-TimeToText(&optional Seconds Universal)
 "Convert the time part of float SECONDS to a string."
 (dkmisc-DateTimeToText Seconds "%H:%M:%S" Universal))

;;;###autoload
(defun dkmisc-DateTimeToText(&optional Seconds Format Universal)
 "Convert float SECONDS (default current) date/time to a string.
Default format is ISO date and time."
 (let*
  ((TimeList (if Seconds (seconds-to-time Seconds) nil))
   (Fs (or Format "%Y-%m-%d %H:%M:%S")))
  (format-time-string Fs TimeList Universal)))

;;;###autoload
(defun dkmisc-TimeParse(Time)
 "Parse string TIME to seconds (floating point)."
 (let*
  ((Old (parse-time-string Time))
   (New nil)
   (Idx 0)
   (Rv nil))
  (dolist (OldElt Old)
   (let*
    ((NewElt OldElt))
    (if (and (null OldElt) (< Idx 3))
     (setq NewElt 0))
    (setq Idx (1+ Idx))
    (setq New (append New (list NewElt)))))
  (setq Rv (float-time (apply 'encode-time New)))

  ; Check for out-of-range input components by converting back to text
  ; and comparing with the original. NOTE: The library functions
  ; accept out-of-range components.
  (let*
   ((Back (dkmisc-DateTimeToText Rv))
    (Mi (string-match
         (concat "^" (regexp-quote Time) "\\(.*\\)") Back))
    (Rest (match-string 1 Back)))
  (if (and Mi
       (or (null Rest) (equal Rest "") (equal Rest ":00")
        (equal Rest " 00:00:00")))
   Rv
   (error "Bad/out of range component in time string: \"%s\"!" Time)))))

(defun dkmisc-TimeDiff(DateStr1 DateStr2)
 "Return difference (secs) between date/time strings DATESTR1 and DATESTR2."
 (let*
  ((T1 (dkmisc-TimeParse DateStr1))
   (T2 (dkmisc-TimeParse DateStr2)))
  (- T1 T2)))

(defun dkmisc-TimeCurrentText(&optional Len)
 "Return the current time in text form, maybe truncated."
 (let*
  ((Rv (dkmisc-DateTimeToText (dkmisc-TimeCurrent))))
  (if Len (setq Rv (substring Rv 0 Len)))
  Rv))

;;;###autoload
(defun dkmisc-TimeCurrent()
 "Return the current time of day as float seconds."
 (float-time (current-time)))

(defconst dkmisc-TimeIntervalRe
 "\\b\\([[:digit:]]+\\)\\([hdwmy]\\)\\b"
 "A time interval.
Also serves as a repetition indicator. A word consisting of
<Digits><Unit> where Unit is y|m|d|w|h.")

(defconst dkmisc-DateTimeRepeaterRe
 (concat dkmisc-DateTimeRe "[[:blank:]]+\\(" dkmisc-TimeIntervalRe "\\)?")
 "A regex for a date/time with optional repetition indicator.")

(defconst dkmisc-TimeShiftRe
 (concat "\\b\\([-+]\\{0,2\\}\\)" dkmisc-TimeIntervalRe)
 "A shifter for a date or time.
Like dkmisc-TimeIntervalRe but with optional sign prefix. Doubled
sign means shift relative to default time, single relative to
current time as per org-mode date input.")

(defun dkmisc-TimeParseShift(Shift)
 "Parse time shift/repetition indicator SHIFT.
Returns a list containing the unit, the count, and t/nil
if the shift is relative to the default/current time."
(let*
 ((Re (dkmisc-FullMatch dkmisc-TimeShiftRe))
  (Rs (or Shift ""))
  (Mi (string-match Re Rs)))
 (if (not Mi)
  (error "Badly formed Time Shift string: \"%s\"!" Shift))
 (let*
  ((Sign (match-string 1 Rs))
   (Digits (match-string 2 Rs))
   (Unit (match-string 3 Rs))
   (FromDefault t)
   (Count (dkmisc-ParseInt Digits)))
  (cond
   ((string= Sign "++"))
   ((string= Sign "--") (setq Count (- Count)))
   ((string= Sign "-") (setq FromDefault nil) (setq Count (- Count)))
   ((string= Sign "+") (setq FromDefault nil))
   ((string= Sign ""))
   (t (error "Badly formed sign in time shift: \"%s\"!" Sign)))
  (list Unit Count FromDefault))))

(defun dkmisc-TimeUnitToSeconds(Unit)
 "Convert time unit UNIT to float seconds.
Returns nil if unit is valid, but conversion is indeterminate.
Error if unit is invalid."
 (let* ((Rv nil))
  (cond
   ((equal "h" Unit) (setq Rv (* 3600.0)))
   ((equal "d" Unit) (setq Rv (* 3600.0 24)))
   ((equal "w" Unit) (setq Rv (* 3600.0 24 7)))
   ((equal "m" Unit))
   ((equal "y" Unit))
   (t (error "Bad time unit: \"%s\"!" Unit)))
  Rv))

(defconst dkmisc-TimeYmdLen 10)
(defconst dkmisc-TimeYmdhmLen 16)
(defconst dkmisc-TimeYmdhmsLen 19)

;;;###autoload
(defun dkmisc-TimeApplyShift(Base Shift)
 "Apply SHIFT to time BASE.
BASE and SHIFT are in text form. Gets expected results for
calendar times by adjusting for daylight savings time if
necessary. If indicated by the shift, BASE is replaced by the
current time. Returns the shifted time in text format with same
length as BASE unless the shift implies more precision is
required."
 (let*
  ((Rv nil)
   (Sl (dkmisc-TimeParseShift Shift))
   (Unit (nth 0 Sl))
   (Count (nth 1 Sl))
   (FromDefault (nth 2 Sl))
   (UnitSeconds (dkmisc-TimeUnitToSeconds Unit))
   (Len (length Base))
   (BaseText (if FromDefault Base (dkmisc-TimeCurrentText Len)))
   (BaseSeconds (dkmisc-TimeParse BaseText)))

  (if UnitSeconds
   ; Shift is exact number of seconds.
   (let*
    ((Shifted (+ BaseSeconds (* UnitSeconds Count)))
     (DstOffsetShifted (dkmisc-TimeZoneOffset Shifted))
     (DstOffset (dkmisc-TimeZoneOffset BaseSeconds)))
    (setq Rv (+ Shifted (- DstOffset DstOffsetShifted))))

   ; Shift is inexact.
   (cond
    ((equal Unit "m") (setq Rv (dkmisc-TimeOffsetMonth BaseSeconds Count)))
    ((equal Unit "y") (setq Rv (dkmisc-TimeOffsetYear BaseSeconds Count)))
    (t (error "Bad Unit: \"%s\"" Unit))))

  (setq Rv (dkmisc-DateTimeToText Rv))
  (if (and (string= Unit "h") (< Len dkmisc-TimeYmdhmLen))
   (setq Len dkmisc-TimeYmdhmLen))
  (setq Rv (substring Rv 0 Len))
  Rv))

(defun dkmisc-TimeOffsetYear(Seconds &optional Offset)
 "Offset float SECONDS by OFFSET (default 1) years.
Limitation: 29 Feb always maps to 28 Feb in the offset date to
avoid a month change, even if the target year is a leap year."
 (let*
  ((Tl (seconds-to-time Seconds))
   (Dt (vconcat (decode-time Tl)))
   (Year (aref Dt 5))
   (Month (aref Dt 4))
   (Day (aref Dt 3))
   (Yo (or Offset 1)))
  (if (not (equal Yo 0))
   (progn
    (aset Dt 5 (+ Year Yo))
    (if (and (equal Month 2) (equal Day 29))
     (aset Dt 3 28))))
  (float-time (apply 'encode-time (append Dt nil)))))

(defun dkmisc-TimeOffsetMonth(Seconds &optional Offset)
 "Offset float SECONDS by OFFSET months.
Limitation: A day of month >28 always offsets to 28 even if the
target month has more days, to avoid amonth change."
 (let*
  ((Tl (seconds-to-time Seconds))
   (Dt (vconcat (decode-time Tl)))
   (Month (aref Dt 4))
   (Day (aref Dt 3))
   (Mo (or Offset 1)))
  (if (not (equal Mo 0))
   (progn
    (aset Dt 4 (+ Month Mo))
    (if (> Day 28) (aset Dt 3 28))))
  (float-time (apply 'encode-time (append Dt nil)))))

(defconst dkmisc-UnexpandedDateRe
 (concat
  "\\(" dkmisc-TwoDigits dkmisc-TwoDigits "\\)\\(" dkmisc-TwoDigits "\\)\\("
  dkmisc-TwoDigits "\\)")
 "Matches an unexpanded date.")

(defun dkmisc-TimeExpandDates()
 "Expand timestamps throughout current buffer by inserting separators."
 (save-excursion
  (goto-char (point-min))
  (while (re-search-forward dkmisc-UnexpandedDateRe nil t)
   (let*
    ((Year (buffer-substring (match-beginning 1) (match-end 1)))
     (Mon (buffer-substring (match-beginning 2) (match-end 2)))
     (Day (buffer-substring (match-beginning 3) (match-end 3)))
     (Replacement (concat Year "-" Mon "-" Day " ")))
   (replace-match Replacement)))))
   
(defconst dkmisc-IntRe "[+-]?[[:digit:]]+"
 "Regular expression matching an integer.")

(defun dkmisc-ParseInt(String)
 "Convert STRING to an integer after validation."
 (let*
  ((Re (dkmisc-FullMatch dkmisc-IntRe))
   (Is (or String ""))
   (Mi (string-match Re Is)))
  (if (null Mi) (error "Badly formed integer: \"%s\"!" String))
  (string-to-number Is)))

(defun dkmisc-FullMatch(Re)
 "Convert RE into a full match for a string."
 (concat "^" Re "$"))

(defvar dkmisc-ReadstringAdviceEnabled nil
 "Set non-nil to enable read-string advice.")

(defvar dkmisc-BaseTimeString nil "Used by read-string advice.")

(defadvice read-string (after dkmisc-ReadstringAdvice activate compile)
 "Augment org input of relative date/time.
Add support for unit \"h\" (hours) and interpret missing sign as
\"+\". Do nothing if `dkmisc-ReadstringAdviceEnabled' is nil."
 (if dkmisc-ReadstringAdviceEnabled
  (let*
   ((Re (concat "^[[:blank:]]*\\(" dkmisc-TimeShiftRe "\\)"))
    (BaseTs
     (if dkmisc-BaseTimeString dkmisc-BaseTimeString
      (dkmisc-TimeCurrentText)))
    (Base (dkmisc-TimeParse BaseTs)))

   ; Empty response?
   (if (string-match "^[[:blank:]]*$" ad-return-value)
    (setq ad-return-value BaseTs)

    ; Relative time shift?
    (if (string-match Re ad-return-value)
     (let*
      ((Rs (match-string 1 ad-return-value)))
      (if Rs
       (let*
        ((New (dkmisc-TimeApplyShift BaseTs Rs)))
        (setq ad-return-value New)))))))))

(defvar dkmisc-OrdgrAdviceEnabled nil
 "Set non-nil to enable org-read-date-get-relative advice.")

(defadvice org-read-date-get-relative
  (before dkmisc-OrdgrAdvice activate compile)
 "Interpret missing sign as ++."
 (if dkmisc-OrdgrAdviceEnabled
  (let*
   ((Input (ad-get-arg 0))
    (Re (concat "^[[:blank:]]*\\(" dkmisc-TimeShiftRe "\\)[[:blank:]]*$")))
   (if (string-match Re Input)
    (let*
     ((Shift (match-string 1 Input))
      (Sign (match-string 2 Input)))
     (if (string= Sign "")
      (ad-set-arg 0 (concat "++" Shift))))))))

(defun dkmisc-TimePromptfor(&optional Base)
 "Read a date/time from the minibuffer.
Return a string (because the length indicates the precision
entered by the user). Interpret relative time shifts from
BASE (a date/time string), or from the current time if BASE is
nil."
 (let*
  ((dkmisc-ReadstringAdviceEnabled t)
   (dkmisc-OrdgrAdviceEnabled t)
   (dkmisc-BaseTimeString Base)
   (SuggestTime nil)
   (Default nil)
   (Rv nil))

  (if Base
   (progn
    (setq Default (seconds-to-time (dkmisc-TimeParse Base)))
    (if (> (length Base) 10) (setq SuggestTime t))))
  (setq Rv (org-read-date SuggestTime nil nil nil Default))
  Rv))

(defun dkmisc-TimeActionable(ScheduledTime SecondsBefore &optional
 IgnoreBefore IgnoreAfter)
"Return t if SCHEDULEDTIME (seconds) is actionable.
SCHEDULEDTIME is actionable when current + SECONDSBEFORE >=
SCHEDULEDTIME. Facilitates taking action when the time has
arrived to deal with something. The optional arguments, numbers
in the range [0,24), specify a range of hours to be excluded from
the calculation. Can be useful to bring the actionable time back
to 'working hours' the previous day."
 (or
  (dkmisc-TimeActionableInternal ScheduledTime SecondsBefore)
  (if (not IgnoreBefore)
   nil
   (if (or (< IgnoreBefore 0) (>= IgnoreAfter 24)
     (< IgnoreAfter 0) (>= IgnoreAfter 24))
    (error "Ignore argument out of range!"))
   (let*
    ((Midnight (dkmisc-TimeToMidnight ScheduledTime))
     (Start nil)
     (End nil)
     (Actionable (+ (dkmisc-TimeCurrent) SecondsBefore)))

    ; Evaluate "dead zone".
    (setq Start (- Midnight (* IgnoreBefore 3600)))
    (setq End (+ Midnight(* IgnoreAfter 3600)))

    (if (and (>= Actionable Start) (<= Actionable End))
     (setq Actionable (+ Actionable (- End Start))))
    (>= Actionable ScheduledTime)))))

(defun dkmisc-TimeToMidnight(Seconds)
 "Returns the nearest Midnight <= the specified time."
 (let*
  ((Tl (seconds-to-time Seconds))
   (Tv (vconcat (decode-time Tl))))
  (aset Tv 0 0)
  (aset Tv 1 0)
  (aset Tv 2 0)
  (float-time (apply 'encode-time (append Tv nil)))))

(defun dkmisc-TimeActionableInternal(Time SecondsBefore)
 "Return t if Time is actionable, ie current + SecondsBefore >= Time."
 (>= (+ (dkmisc-TimeCurrent) SecondsBefore) Time))

(defun dkmisc-InvokeFilter(DiscardOutput Prog &rest Args)
 "Invoke program PROG with arguments ARGS to filter the current buffer.
Discard output if DISCARDOUTPUT is not nil. Buffer contents are
delivered to program's stdin and replaced by program's stdout. If
failure, program's stderr is dislpayed as a message and the
current buffer remains unchanged."
 (let*
  ((Efn (make-temp-file (concat (getenv "TMPDIR") "/emacs.")  nil ".err")))
  ; Ensure temporary error output file is deleted.
  (unwind-protect
   (let*
    ((Ibuf (current-buffer))
     (Ipoint (point)))
    (with-temp-buffer
     ; Temporary output buffer.
     (let* ((Obuf (current-buffer)))
      (save-current-buffer
       (set-buffer Ibuf)
       (let*
        ; Run program.
        ((Ec (apply 'call-process-region (point-min) (point-max) Prog
         nil (list Obuf Efn) nil Args)))

        ; Check for error.
        (if (not (and (integerp Ec) (zerop Ec)))
        (error "Error: %s running %s! Details...\n%s" Ec Prog
         (dkmisc-FileToString Efn)))

        (if (not DiscardOutput)
         ; Swap program output from temporary buffer.
         (progn
          (buffer-swap-text Obuf)

          ; Re-enable undo in ledger buffer.
          (if (equal buffer-undo-list t)
           (setq buffer-undo-list nil))

          ; Try to restore buffer position in new text.
          (goto-char Ipoint))))))))

   ; Remove error file, if any.
   (condition-case nil (delete-file Efn) (error nil)))))

(defun dkmisc-FileToString(Filename)
 "Read file FILENAME and return the content as a string.
Returns nil if the file does not exist."
  (condition-case nil
   (with-temp-buffer
    (insert-file-contents Filename)
    (buffer-string))
   (error nil)))

(defun dkmisc-Matches(Regex List)
 "Returns a list of strings from LIST matching REGEX."
 (let*
  ((Rv nil))
   (dolist (Elem List)
    (if (string-match Regex Elem)
     (add-to-list 'Rv Elem)))
   (nreverse Rv)))

;;;###autoload
(defun dkmisc-FirstMatch(Regex List)
 "Return the first element of LIST matching REGEX, or nil."
 (car (dkmisc-Matches Regex List)))

;;;###autoload
(defun dkmisc-Beep(&optional Count)
 "Beep Count times."
 (let*
  ((Cnt (or Count 1)))
  (while (> Cnt 0)
   (beep)
   (sit-for 0.1)
   (setq Cnt (- Cnt 1)))))

(provide 'dkmisc)    
