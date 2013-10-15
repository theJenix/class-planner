
(defparameter *variables* '(13 14 15 16 17))
(defparameter *domain* '((13 (CS1100 CS1331  CS4495))
                         (14 (CS4001 CS3230))
                         (15 (CS4001 CS4290 CS4605))
                         (16 (CS4641 CS1332))
                         (17 (CS4001))))

;; General purpose methods for testing and managing
;; the assignment.
(defun potential-assignments (var domain)
    (cadr (assoc var domain)))

(defun is-complete (assignment)
    (format t "Is variable assigned? ~a~C" (mapcar #'not (mapcar #'null assignment)) #\linefeed)
    (notany #'null assignment))

(defun empty-assignment (variables)
    (loop for v in variables collect nil))

(defun is-value-consistent (value var assignment variables)
    (and (null (position value assignment))
         (null (nth (position var variables) assignment))))

(defun first-variable (variables)
    (car variables))

(defun next-variable (assignment variables)
    (nth (position nil assignment) variables))

(defun inference (value var variables)
    (let ((infer (empty-assignment variables))
          (next  (1+ (position var variables))))
        (when (< next (length variables))
            (setf (nth next infer) 'BREAK))
        infer))


;; Direct implementation of AIMA algorithm (AIMA 215)
;; Note: this implementation is more complicated because it
;; manages state in place (instead of in a more pure functional
;; way).  See below for an alternate implementation.
(defun assign (value var assignment variables)
    (format t "Assigning ~s to ~s~C" var value #\linefeed)
    (setf (nth (position var variables) assignment) value)
    assignment)

(defun unassign (var assignment variables)
    (assign nil var assignment variables))

(defun apply-inference (inference assignment)
    (loop for i in inference
          for n below (length assignment) do
        (when i (setf (nth n assignment) i))))

(defun remove-inference (inference assignment)
    (loop for i in inference
          for n below (length assignment) do
        (when i (setf (nth n assignment) nil))))

(defun do-backtrack (assignment variables domain)
    (if (is-complete assignment)
        (progn (format t "Complete assignment!~C" #\linefeed) assignment)
        (let ((var (next-variable assignment variables)))
            (format t "Processing ~s~C" var #\linefeed)
            (loop with potentials = (potential-assignments var domain)
                  for potential in potentials 
                  when (is-value-consistent potential var assignment variables) do
                      (progn (assign potential var assignment variables)
                           (let ((infer (inference potential var variables)))
                               (if infer
                                   (progn
                                       (apply-inference infer assignment)
                                       (if (do-backtrack assignment variables domain)
                                           (return-from do-backtrack assignment)
                                           (progn
                                               (unassign var assignment variables)
                                               (remove-inference infer assignment))
                                       ))))))
            (format t "~a inconsistent with ~a~C" (potential-assignments var domain) assignment #\linefeed)
            nil
        )))

;; Main entrypoint
(defun backtrack (variables domain)
    (do-backtrack (empty-assignment variables) variables domain))

;; Pure functional implementation of backtracking search.  This is a
;; slightly different algorithm than presented in the book, but is
;; functionally equivalent.

(defun apply-inference-f (inference assignment)
    (mapcar (lambda (i a) (if i i a)) inference assignment))

(defun assignf (value var assignment variables)
    (let ((copy (copy-seq assignment)))
        (format t "Assigning ~s to ~s~C" var value #\linefeed)
        (setf (nth (position var variables) copy) value)
        copy))

(defun make-consistentp (var assignment variables)
    (lambda (p) (when (is-value-consistent p var assignment variables) p)))

(defun make-map-inference (var variables)
    (lambda (p) (inference p var variables)))

(defun do-backtrack-f (var assignment variables domain)
    ; filter the list of potential assignments by ones that are consistent with this assignment
    (let ((potentials (potential-assignments var domain))
          (consistent)
          (infers)
          (good nil))
          (setf consistent (remove-if-not (make-consistentp   var assignment variables) potentials))
          (setf infers     (mapcar        (make-map-inference var variables) potentials))
          (format t "Processing ~s~C" var #\linefeed)
          (if (not consistent)
              (progn
                  (format t "~a inconsistent with ~a~C" potentials assignment #\linefeed)
                  nil)
              (progn
                  (loop for potential in consistent
                        for infer in infers
                        while (not good) do
                      (let ((candidate (apply-inference-f infer (assignf potential var assignment variables))))
                           (if (is-complete candidate)
                               (setf good (progn (format t "Complete assignment!~C" #\linefeed) candidate))
                               (setf good (do-backtrack-f (next-variable candidate variables) candidate variables domain)))))
                  good))))


;; Main entrypoint
(defun backtrack-f (variables domain)
  (let ((assignment (empty-assignment variables)))
      (if (is-complete assignment)
          (progn (format t "Complete assignment!~C" #\linefeed) assignment)
          (do-backtrack-f (first-variable variables) (empty-assignment variables) variables domain))))

(format t "--AIMA algorithm--~C" #\linefeed)
(format t "~a~C~C" (backtrack *variables* *domain*) #\linefeed #\linefeed)
(format t "--Lispy algorithm--~C" #\linefeed)
(format t "~a~C~C" (backtrack-f *variables* *domain*) #\linefeed #\linefeed)
