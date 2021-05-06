class Point:
    x = 0
    y = 0
    cluster_number = 0

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def set_cluster_number(self, cluster_number):
        self.cluster_number = cluster_number
#print(Point(1,1))