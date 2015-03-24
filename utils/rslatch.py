#!/usr/bin/env python3

"""R-S latch, with triggers on rising and falling edge, and status querying.
"""

class RSLatch(object):
    """
    The default implementation never triggers.

    Implement up_trigger(self, item) and down_trigger(self, item)
    to get a useful latch.
    """
    def __init__(self, initial_state=False):
        self._state = initial_state

    def up_trigger(self, item):
        """Return whether the given item should switch the latch on.

        Your subclass should override this.
        """
        return False

    def down_trigger(self, item):
        """Return whether the given item should switch the latch off.

        Your subclass should override this.
        """
        return False

    def feed(self, item):
        """Feed an item into the latch's input.
        """
        if (not self._state) and self.up_trigger(item):
            self._state = True
            self.rising_edge(item)
        elif self._state and self.down_trigger(item):
            self._state = False
            self.falling_edge(item)

    def is_on(self):
        return self._state

    def is_off(self):
        return not self.is_on()


    def rising_edge(self, item):
        """Override to perform some operation on the rising edge.

        item: The item that is triggering the rising edge
        """
        return

    def falling_edge(self, item):
        """Override to perform some operation on the falling edge.

        item: The item that is triggering the falling edge
        """
        return
