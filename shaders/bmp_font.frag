#version 330 core

out vec4 FragColor;

int vec2 texCoord;

uniform sampler2D fontTexture;

void main()
{
  FragColor = texture(fontTexture, texCoord);
}
