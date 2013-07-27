; Miscellaneous utility functions in Emacs Lisp.    
; (c) David Keegan 20031209

(defun dkMisc()
"Package of miscellaneous utility functions in Emacs Lisp.

For more information see:
 `dkmStringTrimWs'
 `dkmDocstringsExtract'."
 (interactive)
 (describe-function 'dkMisc))

(defgroup dkDk nil
"David Keegan's Group."
 :version "21.2"
 :tag "David Keegan"
 :group 'emacs)

(defgroup dkMisc nil
"Miscellaneous Utilities."
 :version "21.2"
 :tag "Miscellaneous Utilities"
 :group 'dkDk)

(defun dkmStringTrimWs(String)
 "Strips whitespace from the beginning and end of STRING"
 (let* (Result)
  (setq Result String)
  (and (string-match "^[ \t\n]*" Result)
   (setq Result (replace-match "" t t Result)))
  (and (string-match "[ \t\n]*$" Result)
   (setq Result (replace-match "" t t Result)))
  Result))

(defun dkmDocstringGet(Symbol)
"Returns the documentation associated with SYMBOL or nil if none."
 (interactive)
 ; Function?
 (if (fboundp Symbol)
   (documentation Symbol)
   ; Variable?
   (if (boundp Symbol)
    (documentation-property Symbol 'variable-documentation))))


(defun dkmDocstringShowMatching(Pattern)
"Extracts the documentation text for symbols matching PATTERN.
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
     ((Sn (symbol-name Symbol)))
     (setq Flag (make-string (- 60 (length Sn)) ?.))
     (princ
      (format "\n\n\n%s %s%s\n%s"
       (if (fboundp Symbol) "Function" "Variable")
       Sn Flag (or (dkmDocstringGet Symbol) "Not Documented!"))))))))

(defconst dkmConflictMarkerRe
 "^<<<<<<"
 "Matches a merge conflict marker.")

(defconst dkmTwoDigits
 "[0-9]\\{2\\}"
"Matches any two digits.")

(defconst dkmYearRe
 (concat
  dkmTwoDigits
  "?"
  dkmTwoDigits)
"Matches a two or four digit year.")

(defconst dkmYearMonthRe
 (concat
  "\\("
  dkmYearRe
  "\\)"
  "-?"
  "\\("
  dkmTwoDigits
  "\\)")
"Matches a Year and Month with optional punctuation.")

(defconst dkmDateRe
 (concat
  dkmYearMonthRe
  "-?"
  "\\("
  dkmTwoDigits
  "\\)")
"Matches an ISO date/time with optional punctuation.")

(defconst dkmTimeRe
 (concat
  dkmTwoDigits
  ":?"
  dkmTwoDigits
  "\\("
  ":?"
  dkmTwoDigits
  "\\)?")
"Matches an ISO time with optional seconds and optional punctuation.")

(defconst dkmDateTimeRe
 (concat
  "\\("
  dkmDateRe
  "\\)"
  "\\("
  "[[:blank:]]"
  "\\("
  dkmTimeRe
  "\\)"
  "\\)?")
"Matches an ISO date/time with optional time and optional punctuation.")

(defun dkmTimeZoneOffset(Seconds)
"Returns the timezone offset in seconds of UTC float time Seconds."
 (car (current-time-zone (seconds-to-time Seconds))))

(defun dkmTimeZoneLabel(Seconds)
"Returns the timezone label of UTC float time Seconds."
 (nth 1 (current-time-zone (seconds-to-time Seconds))))

(defconst dkmCurrentTimeShift nil
"Time shift for test purposes in float seconds.
Set via dkmSetCurrentTimeShift.")

(defun dkmSetCurrentTimeShift (&optional DateTimeString)
"Causes current-time to shift the value returned.
The value returned increments from the specified time. Call with
nil argument to eliminate the shift and return to normal.
WARNING: May have unexpected side-effects. Use for test purposes
only. Affects the appearance of the emacs cursor."
 ; Eliminate any existing time shift.
 (setq dkmCurrentTimeShift nil)
 (if DateTimeString
  (let*
   ((Current (dkmTimeCurrent))
    (Target (dkmTimeParse DateTimeString)))
   (setq dkmCurrentTimeShift (- Target Current))))
 dkmCurrentTimeShift)

(defadvice current-time (after dkCt activate compile)
"Shifts returned value for test purposes."
 (if dkmCurrentTimeShift
  (let* ((Current (float-time ad-return-value)))
   (setq ad-return-value
    (seconds-to-time (+ Current dkmCurrentTimeShift))))))

(defun dkmDateToText(&optional Seconds Universal)
"Converts the date part of float Seconds to a string."
 (dkmDateTimeToText Seconds "%Y-%m-%d" Universal))

(defun dkmTimeToText(&optional Seconds Universal)
"Converts the time part of float Seconds to a string."
 (dkmDateTimeToText Seconds "%H:%M:%S" Universal))

(defun dkmDateTimeToText(&optional Seconds Format Universal)
"Converts float seconds (default current) date/time to string.
 Default format is ISO date and time."
 (let*
  ((TimeList (if Seconds (seconds-to-time Seconds) nil))
   (Fs (or Format "%Y-%m-%d %H:%M:%S")))
  (format-time-string Fs TimeList Universal)))

(require 'parse-time)

(defun dkmTimeParse(Time)
"Parses string Time to seconds (floating point)."
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
  (setq Rv (time-to-seconds (apply 'encode-time New)))

  ; Check for out-of-range input components by converting back to text
  ; and comparing with the original. NOTE: The library functions
  ; accept out-of-range components.
  (let*
   ((Back (dkmDateTimeToText Rv))
    (Mi (string-match
         (concat "^" (regexp-quote Time) "\\(.*\\)") Back))
    (Rest (match-string 1 Back)))
  (if (and Mi
       (or (null Rest) (equal Rest "") (equal Rest ":00")
        (equal Rest " 00:00:00")))
   Rv
   (error "Bad/out of range component in time string: \"%s\"!" Time)))))

(defun dkmTimeDiff(DateStr1 DateStr2)
"Returns the difference in seconds between two date/time strings."
 (let*
  ((T1 (dkmTimeParse DateStr1))
   (T2 (dkmTimeParse DateStr2)))
  (- T1 T2)))

(defun dkmTimeCurrentText(&optional Len)
"Returns the current time in text form, maybe truncated."
 (let*
  ((Rv (dkmDateTimeToText (dkmTimeCurrent))))
  (if Len (setq Rv (substring Rv 0 Len)))
  Rv))

(defun dkmTimeCurrent()
"Current time of day as float seconds."
 (float-time (current-time)))

(defconst dkmTimeRepeaterRe
 "\\b\\([[:digit:]]+\\)\\([hdwmy]\\)\\b"
"A repetition indicator for a date or time.
 A word consisting of <Digits><Unit> where Unit is y|m|d|w|h.")

(defconst dkmDateTimeRepeaterRe
 (concat dkmDateTimeRe "[[:blank:]]+\\(" dkmTimeRepeaterRe "\\)?")
"A regex for a date/time with optional repeater.")

(defconst dkmTimeShiftRe
 (concat "\\b\\([-+]\\{0,2\\}\\)" dkmTimeRepeaterRe)
"A shifter for a date or time.
 Like dkmTimeRepeaterRe but with optional sign prefix. Doubled
 sign means shift relative to default time, single relative to
 current time as per org-mode date input.")

(defun dkmTimeParseShift(Shift)
"Parses a time shift/repetition indicator.
 Returns a list containing the unit, the count, and t/nil
 if the shift is relative to the default/current time."
(let*
 ((Re (dkmFullMatch dkmTimeShiftRe))
  (Rs (or Shift ""))
  (Mi (string-match Re Rs)))
 (if (not Mi)
  (error "Badly formed Time Shift string: \"%s\"!" Shift))
 (let*
  ((Sign (match-string 1 Rs))
   (Digits (match-string 2 Rs))
   (Unit (match-string 3 Rs))
   (FromDefault t)
   (Count (dkmParseInt Digits)))
  (cond
   ((string= Sign "++"))
   ((string= Sign "--") (setq Count (- Count)))
   ((string= Sign "-") (setq FromDefault nil) (setq Count (- Count)))
   ((string= Sign "+") (setq FromDefault nil))
   ((string= Sign ""))
   (t (error "Badly formed sign in time shift: \"%s\"!" Sign)))
  (list Unit Count FromDefault))))

(defun dkmTimeUnitToSeconds(Unit)
"Converts a time unit to float seconds.
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

(defconst dkmTimeYmdLen 10)
(defconst dkmTimeYmdhmLen 16)
(defconst dkmTimeYmdhmsLen 19)

(defun dkmTimeApplyShift(Base Shift)
"Applies a shift to a time in text form.
 Shift is also in text format. Gets expected results for calendar
 times by adjusting for daylight savings time if necessary. If
 indicated by the shift, Base is replaced by the current time.
 Returns the shifted time in text format with same length as Base
 unless the shift implies more precision is required."
 (let*
  ((Rv nil)
   (Sl (dkmTimeParseShift Shift))
   (Unit (nth 0 Sl))
   (Count (nth 1 Sl))
   (FromDefault (nth 2 Sl))
   (UnitSeconds (dkmTimeUnitToSeconds Unit))
   (Len (length Base))
   (BaseText (if FromDefault Base (dkmTimeCurrentText Len)))
   (BaseSeconds (dkmTimeParse BaseText)))

  (if UnitSeconds
   ; Shift is exact number of seconds.
   (let*
    ((Shifted (+ BaseSeconds (* UnitSeconds Count)))
     (DstOffsetShifted (dkmTimeZoneOffset Shifted))
     (DstOffset (dkmTimeZoneOffset BaseSeconds)))
    (setq Rv (+ Shifted (- DstOffset DstOffsetShifted))))

   ; Shift is inexact.
   (cond
    ((equal Unit "m") (setq Rv (dkmTimeOffsetMonth BaseSeconds Count)))
    ((equal Unit "y") (setq Rv (dkmTimeOffsetYear BaseSeconds Count)))
    (t (error "Bad Unit: \"%s\"" Unit))))

  (setq Rv (dkmDateTimeToText Rv))
  (if (and (string= Unit "h") (< Len dkmTimeYmdhmLen))
   (setq Len dkmTimeYmdhmLen))
  (setq Rv (substring Rv 0 Len))
  Rv))

(defun dkmTimeOffsetYear(Seconds &optional Offset)
"Offsets float Seconds by Offset (default 1) years.
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
  (time-to-seconds (apply 'encode-time (append Dt nil)))))

(defun dkmTimeOffsetMonth(Seconds &optional Offset)
"Offsets float Seconds by Count months.
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
  (time-to-seconds (apply 'encode-time (append Dt nil)))))

(defconst dkmUnexpandedDateRe
 (concat "\\(" dkmTwoDigits dkmTwoDigits "\\)\\(" dkmTwoDigits "\\)\\("
  dkmTwoDigits "\\)")
"Matches an unexpanded date.")

(defun dkmTimeExpandDates()
"Expands timestamps throughout the buffer by inserting missing separators."
 (save-excursion
  (goto-char (point-min))
  (while (re-search-forward dkmUnexpandedDateRe nil t)
   (let*
    ((Year (buffer-substring (match-beginning 1) (match-end 1)))
     (Mon (buffer-substring (match-beginning 2) (match-end 2)))
     (Day (buffer-substring (match-beginning 3) (match-end 3)))
     (Replacement (concat Year "-" Mon "-" Day " ")))
   (replace-match Replacement)))))
   
(defconst dkmIntRe "[+-]?[[:digit:]]+"
"Regular expression matching an integer.")

(defun dkmParseInt(String)
"Converts String to an integer after validation."
 (let*
  ((Re (dkmFullMatch dkmIntRe))
   (Is (or String ""))
   (Mi (string-match Re Is)))
  (if (null Mi) (error "Badly formed integer: \"%s\"!" String))
  (string-to-number Is)))

(defun dkmFullMatch(Re)
"Convert Regexp into a full match for a string."
 (concat "^" Re "$"))

(defvar dkmReadstringAdviceEnabled nil
 "Set non-nil to enable read-string advice.")

(defvar dkmBaseTimeString nil "Used by read-string advice.")

(defadvice read-string (after dkmReadstringAdvice activate compile)
"Augments org input of relative date/time.
Adds support for unit \"h\" (hours) and interprets missing sign
as \"+\". Does nothing if dkmReadstringAdviceEnabled is nil."
 (if dkmReadstringAdviceEnabled
  (let*
   ((Re (concat "^[[:blank:]]*\\(" dkmTimeShiftRe "\\)"))
    (BaseTs
     (if dkmBaseTimeString dkmBaseTimeString
      (dkmTimeCurrentText)))
    (Base (dkmTimeParse BaseTs)))

   ; Empty response?
   (if (string-match "^[[:blank:]]*$" ad-return-value)
    (setq ad-return-value BaseTs)

    ; Relative time shift?
    (if (string-match Re ad-return-value)
     (let*
      ((Rs (match-string 1 ad-return-value)))
      (if Rs
       (let*
        ((New (dkmTimeApplyShift BaseTs Rs)))
        (setq ad-return-value New)))))))))

(defvar dkmOrdgrAdviceEnabled nil
 "Set non-nil to enable org-read-date-get-relative advice.")

(defadvice org-read-date-get-relative (before dkmOrdgrAdvice activate compile)
"Interprets missing sign as ++."
 (if dkmOrdgrAdviceEnabled
  (let*
   ((Input (ad-get-arg 0))
    (Re (concat "^[[:blank:]]*\\(" dkmTimeShiftRe "\\)[[:blank:]]*$")))
   (if (string-match Re Input)
    (let*
     ((Shift (match-string 1 Input))
      (Sign (match-string 2 Input)))
     (if (string= Sign "")
      (ad-set-arg 0 (concat "++" Shift))))))))

(defun dkmTimePromptfor(&optional Base)
 "Reads a date/time from the minibuffer.
Returns a string (because the length indicates the precision
entered by the user. Interprets relative time shifts from
Base (a date/time string), or current time if Base is nil."
 (let*
  ((dkmReadstringAdviceEnabled t)
   (dkmOrdgrAdviceEnabled t)
   (dkmBaseTimeString Base)
   (SuggestTime nil)
   (Default nil)
   (Rv nil))

  (if Base
   (progn
    (setq Default (seconds-to-time (dkmTimeParse Base)))
    (if (> (length Base) 10) (setq SuggestTime t))))
  (setq Rv (org-read-date SuggestTime nil nil nil Default))
  Rv))

(defun dkmTimeDue(ScheduledTime SecondsBefore &optional
 IgnoreBefore IgnoreAfter)
"Returns t if ScheduledTime (seconds) is 'due'.
A Scheduled Time is 'due' when CurrentScheduledTime +
SecondsBefore >= ScheduledTime. Used to give a warning when the
time has arrived to deal with something. The optional arguments,
numbers in the range [0,24), specify a range of hours to be
excluded from the calculation. Can be useful to bring the 'due'
time back to 'working hours' the previous day."
 (or
  (dkmTimeDueInternal ScheduledTime SecondsBefore)
  (if (not IgnoreBefore)
   nil
   (if (or (< IgnoreBefore 0) (>= IgnoreAfter 24)
     (< IgnoreAfter 0) (>= IgnoreAfter 24))
    (error "Ignore argument out of range!"))
   (let*
    ((Midnight (dkmTimeToMidnight ScheduledTime))
     (Start nil)
     (End nil)
     (Due (+ (dkmTimeCurrent) SecondsBefore)))

    ; Evaluate "dead zone".
    (setq Start (- Midnight (* IgnoreBefore 3600)))
    (setq End (+ Midnight(* IgnoreAfter 3600)))

    (if (and (>= Due Start) (<= Due End))
     (setq Due (+ Due (- End Start))))
    (>= Due ScheduledTime)))))

(defun dkmTimeToMidnight(Seconds)
"Returns the nearest Midnight <= the specified time."
 (let*
  ((Tl (seconds-to-time Seconds))
   (Tv (vconcat (decode-time Tl))))
  (aset Tv 0 0)
  (aset Tv 1 0)
  (aset Tv 2 0)
  (time-to-seconds (apply 'encode-time (append Tv nil)))))

(defun dkmTimeDueInternal(Time SecondsBefore)
"Returns t if Time is 'due', ie CurrentTime + SecondsBefore >= Time.
Used to give a warning when the time has arrived to deal with
something."
 (>= (+ (dkmTimeCurrent) SecondsBefore) Time))

(defun dkmInvokeFilter(DiscardOutput Prog &rest Args)
 "Invokes a program to filter the current buffer.
  Contents are delivered to program's stdin and replaced by
  program's stdout. If failure, program's stderr is dislpayed as a
  message and the current buffer remains unchanged."
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
         (dkmFileToString Efn)))

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

(defun dkmFileToString(Filename)
 "Reads a file and returns the content as a string.
  Returns nil if the file does not exist."
  (condition-case nil
   (with-temp-buffer
    (insert-file-contents Filename)
    (buffer-string))
   (error nil)))

(defun dkmMatches(Regex List)
"Returns a list of strings from List matching Regex."
 (let*
  ((Rv nil))
   (dolist (Elem List)
    (if (string-match Regex Elem)
     (add-to-list 'Rv Elem)))
   (nreverse Rv)))

(defun dkmFirstMatch(Regex List)
 "Returns first element of List matching Regex, or nil."
 (car (dkmMatches Regex List)))

(provide 'dkMisc)    
