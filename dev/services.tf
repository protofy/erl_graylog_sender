provider "docker" {

}

resource "docker_image" "mongo" {
  name = "mongo"
  keep_updated = true
}

resource "docker_container" "mongo" {
  name = "mongo"
  image = "mongo"
}

resource "docker_image" "graylog" {
  name = "graylog2/allinone"
  keep_updated = true
}

resource "docker_container" "graylog" {
  image = "graylog2/allinone"
  name = "graylog"
  depends_on = ["docker_container.mongo"]
  ports = {
    internal = 9000
    external = 9000
  }
  ports = {
    internal = 12201
    external = 12201
  }
  ports = {
    internal = 12201
    external = 12201
    protocol = "udp"
  }
}


