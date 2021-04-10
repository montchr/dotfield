variable "branch" {
  description = "Git branch"
  default = "main"
}

variable "image_id" {
  description = "Source image ID"
  default = "linode/ubuntu20.04"
}

variable "authorized_keys" {
  description = "Authorized SSH public keys"
}

variable "root_pass" {
  description = "Root password for the linode"
}

variable "region" {
  description = "Linode region"
  default = "us-east"
}
