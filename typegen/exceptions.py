class SemanticException(Exception):
    def __init__(self, message):
        super(SemanticException, self).__init__(message)
