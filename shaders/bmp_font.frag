#version 330 core

out vec4 FragColor;

in vec2 texCoord;

uniform sampler2D fontTexture;

void main()
{
  vec3 color = texture(fontTexture, texCoord).rgb;

  if (color.r < 0.5) {
    FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  } else {
    FragColor = vec4(0.0, 0.0, 0.0, 0.3);
  }
}
