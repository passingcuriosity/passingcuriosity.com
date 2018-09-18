terraform {
  backend "s3" {
    bucket  = "thsutton-cfg"
    key     = "passingcuriosity/terraform.tfstate"
    encrypt = "true"
    region  = "ap-southeast-2"
  }
}

provider "archive" {
  version = "~> 1.1"
}

provider "aws" {
  version = "~> 1.36"
  region  = "ap-southeast-2"
}

provider "aws" {
  version = "~> 1.36"
  region  = "us-east-1"
  alias   = "global"
}
