(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defparameter *suffixes*
  '(Jr Junior Sr Senior MD I II II III IV V VI VII VIII IX X)
  "A list of suffixes that may be appenended to a name.")

(defun last-name (name)
  "Returns the last name from a name represented as a list."
  (if (member (first (last name)) *suffixes*)
      (last-name (reverse (rest (reverse name))))
      (first (last name))))
