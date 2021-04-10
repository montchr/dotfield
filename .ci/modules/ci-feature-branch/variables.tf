# ================================
# Required
# ================================

variable "authorized_keys" {
  description = "Authorized SSH public keys"
}

variable "image_id" {
  description = "Source image ID"
}

variable "root_pass" {
  description = "Root password for the linode"
}


# ================================
# Optional
# ================================

variable "branch" {
  description = "Git branch"
  default     = "main"
}

variable "region" {
  description = "Linode region"
  default     = "us-east"
}
