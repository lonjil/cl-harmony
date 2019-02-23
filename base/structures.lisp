(in-package #:xyz.lonjil.discord/base)

;;;(defun save-file )


(defclass guild ()
  ((%id :accessor id :initarg :id :type string :json-type :string
        :json-key "id")
   (%name :accessor name :initarg :name :type string :json-type :string
          :json-key "name")
   (%owner_id :accessor owner-id :initarg :owner-id :type string
              :json-type :string :json-key "owner_id"))
  (:metaclass json-mop:json-serializable-class))

(defclass channel ()
  ((%id :accessor id :initarg :id :type string
        :json-type :string :json-key "id")
   (%type :accessor type :initarg :type :type number
          :json-type :number :json-key "type")
   (%guild_id :accessor guild-id :initarg :guild-id :type (or nil string)
              :json-type :string :json-key "guild_id")
   (%name :accessor name :initarg :name :type string
          :json-type :string :json-key "name"))
  (:metaclass json-mop:json-serializable-class))

(defclass user ()
  ((%id :accessor id :initarg :id :type string :json-type :string
        :json-key "id")
   (%username :accessor username :initarg :username :type string
              :json-type :string :json-key "username")
   (%discriminator :accessor discrim :initarg :discrim :type string
                   :json-type :string :json-key "discriminator")
   (%avatar :accessor avatar :initarg :avatar :type :string
            :json-type :string :json-key avatar)
   (%bot :accessor bot? :initarg :bot? :type boolean
         :json-type :bool :json-key "bot"))
  (:metaclass json-mop:json-serializable-class))

(defclass invite ()
  ((%code :accessor code :initarg :code :type string
          :json-type :string :json-key "code")
   (%guild :accessor guild :initarg :guild :type guild
           :json-type guild :json-key "guild")
   (%channel :accessor channel :initarg :channel :type channel
             :json-type channel :json-key "channel")
   (%inviter :accessor inviter :initarg :inviter :type user
             :json-type user :json-key "inviter")
   (%uses :accessor uses :initarg :uses :type number
          :json-type :number :json-key "uses"))
  (:metaclass json-mop:json-serializable-class))
(defclass guild-member ()
  ((%user :accessor user :initarg :user :type user
          :json-type user :json-key "user")
   (%nick :accessor nick :initarg :nick :type string
          :json-type :string :json-key "nick")
   (%roles :accessor roles :initarg :roles)
   (%guild_id :accessor guild-id :initarg :guild-id
              :json-type :string :json-key "guild_id")
   (%joined_at :accessor joined-at :initarg :joined-at))
  (:metaclass json-mop:json-serializable-class))

(defclass message ()
  ((%id :accessor id :initarg :id :type string :json-type :string
        :json-key "id")
   (%channel_id :accessor channel-id :initarg :channel-id :type string
                :json-type :string :json-key "channel_id")
   (%guild_id :accessor guild-id :initarg :guild-id :type string
              :json-type :string :json-key "guild_id")
   (%author :accessor author :initarg :author :type user
            :json-type user :json-key "author")
  #+(or)(%member :accessor guild-member :initarg :guild-member :type guild-member
            :json-type guild-member :json-key "member")
   (%content :accessor content :initarg :content :type string
             :json-type :string :json-key "content")
   (%timestamp :accessor timestamp :initarg :timestamp))
  (:metaclass json-mop:json-serializable-class))


(defclass payload ()
  ((%op :accessor op :initarg :op :type number
        :json-key "op" :json-type :number)
   (%d :accessor data :initarg :data
       :json-key "d" :json-type :hash-table)
   (%s :accessor seq :initarg :seq :type number
       :json-key "s" :json-type :number)
   (%t :accessor event :initarg :event :type string
       :json-key "t" :json-type :string))
  (:metaclass json-mop:json-serializable-class))


(defclass properties ()
  ((%$os :accessor os :initarg :os :type string
         :json-type :string :json-key "$os")
   (%$browser :accessor browser :initarg :browser :type string
              :json-type :string :json-key "$browser")
   (%$device :accessor device :initarg :device :type string
             :json-type :string :json-key "$device"))
  (:metaclass json-mop:json-serializable-class))
(defclass activity ()
  ((%name :accessor name :initarg :name :type string
          :json-type :string :json-key "name")
   (%type :accessor type :initarg :type :type number
          :json-type :number :json-key "type"))
  (:metaclass json-mop:json-serializable-class))
(defclass update-status ()
  ((%since :accessor since :initarg :since :type number
           :json-type :number :json-key "since")
   (%game :accessor game :initarg :game :type activity
          :json-type activity :json-key "game")
   (%status :accessor status :initarg :status :type string
            :json-type :string :json-key "status")
   (%afk :accessor afk? :initarg :afk? :type boolean
         :json-type :bool :json-key "afk"))
  (:metaclass json-mop:json-serializable-class))
(defclass identify ()
  ((%token :accessor token :initarg :token :type string
           :json-type :string :json-key "token")
   (%properties :accessor properties :initarg :properties :type properties
                :json-type properties :json-key "properties")
   (%presence :accessor presence :initarg :presence :type update-status
              :json-type update-status :json-key "presence"))
  (:metaclass json-mop:json-serializable-class))

(defclass hello ()
  ((%heartbeat_interval :accessor heartbeat-interval :initarg :heartbeat-interval
                        :json-type :number :json-key "heartbeat_interval")
   (%_trace :accessor _trace :initarg :_trace :json-type :list
            :json-key "_trace"))
  (:metaclass json-mop:json-serializable-class))

