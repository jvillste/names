(ns names.space)

(defprotocol Space
  (size [space])
  (coordinates [space index])
  (index [space coordinates]))

