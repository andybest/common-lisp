(in-package #:net.mfiano.lisp.dungen)

(defun filter-connectable (kernel)
  (and (not (carved-p (select kernel 0 0)))
       (or (cell-regions-distinct-p (select kernel 0 1) (select kernel 0 -1))
           (cell-regions-distinct-p (select kernel 1 0) (select kernel -1 0)))))

(defun make-connector (kernel)
  (let ((cell (select kernel 0 0))
        (regions (remove 0 (kernel-map kernel #'cell-region))))
    (add-feature cell :connector)
    (push cell (u:href (state-connections *state*) regions))))

(defun connect-regions (stage)
  (convolve stage (layout :orthogonal) #'filter-connectable #'make-connector))

(defun connectable-edges ()
  (let (edges)
    (u:do-hash-keys (k (state-connections *state*))
      (push (cons k 1) edges))
    edges))

(defun make-graph ()
  (graph:populate (make-instance 'graph:graph)
                  :nodes (u:iota (hash-table-count (state-regions *state*))
                                 :start 1)
                  :edges-w-values (connectable-edges)))

(defun make-tree ()
  (let ((graph (make-graph)))
    (graph/graph:minimum-spanning-tree graph graph)))

(defun adjacent-junction-p (kernel)
  (kernel-detect kernel (lambda (x) (feature-intersect x :junction :door))))

(defun generate-junction-feature (stage)
  (if (random-boolean (state-rng *state*) (stage-door-rate stage))
      :door
      :junction))

(defun remove-connectors (kernel)
  (kernel-map kernel (lambda (x) (remove-feature x :connector))))

(defun maybe-make-junction (stage cell)
  (let ((kernel (cell->kernel stage cell (layout :orthogonal))))
    (unless (adjacent-junction-p kernel)
      (carve cell (generate-junction-feature stage))
      (remove-connectors kernel)
      (cond ((cell-regions-distinct-p (select kernel 0 1) (select kernel 0 -1))
             (add-feature cell :door/horizontal))
            ((cell-regions-distinct-p (select kernel 1 0) (select kernel -1 0))
             (add-feature cell :door/vertical))))))

(defun get-random-edge-connector (edge)
  (random-element (state-rng *state*)
                  (u:href (state-connections *state*) edge)))

(defun carve-junctions (stage)
  (loop :with graph = (make-tree)
        :for edge :in (graph:edges graph)
        :do (maybe-make-junction stage (get-random-edge-connector edge))
        :when (random-boolean (state-rng *state*) (stage-cycle-factor stage))
          :do (maybe-make-junction stage (get-random-edge-connector edge))))
