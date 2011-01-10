MOB.ItemRegistry = function() {
    var uriToReferenceMap = {};
    var totalItems = 0;

    return {
        /**
         *
         * @param item
         * @param id a globally unique identifier for the subscriber
         * @param onChangedCallback a function to execute when the item is updated
         */
        subscribeToItem: function(item, id, onChangedCallback) {
            var ref = uriToReferenceMap[item.uri];

            if (null == ref) {
                //alert("registering item");
                ref = {};
                ref.item = item;
                ref.callbacks = {};
                ref.count = 0;
                uriToReferenceMap[item.uri] = ref;
                totalItems++;
            }

            ref.callbacks[id] = onChangedCallback;
            ref.count++;
        },

        unsubscribeFromItem: function(item, id) {
            var ref = uriToReferenceMap[item.uri];

            if (null != ref) {
                delete ref.callbacks[id];
                ref.count--;

                if (0 == ref.count) {
                    delete uriToReferenceMap[item.uri];
                    totalItems--;
                }
            } else {
                alert("not subscribed to item");
            }
        },

        updateItem: function(item) {
            var ref = uriToReferenceMap[item.uri];

            // It is permissible for an item to be updated which is not yet registered.
            if (null != ref) {
                ref.item = item;

                for (var id in ref.callbacks) {
                    ref.callbacks[id](item);
                }
            }
        },

        size: function() {
            return totalItems;
        }
    };
};
